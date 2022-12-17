{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE RecordWildCards    #-}
import Control.Applicative      ( (<|>), many )
                 
import Data.Attoparsec.Text     ( Parser, endOfInput, parseOnly, char )
import Data.Set                 ( Set )
import qualified Data.Set       as Set 
import qualified Data.Text.IO   as TIO

import System.Environment       ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            jetPattern <- readInput ((many parseJet) <* endOfInput) inputFilePath            
            putStrLn "[Part 1] height after 2022 rocks:"
            print $ height $ simulateFall 2022 (cycle jetPattern)

   
    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)
        
        parseJet :: Parser Jet
        parseJet = char '<' *> pure JLeft <|> char '>' *> pure JRight


data Jet = JLeft | JRight
    deriving (Eq, Show)

newtype X = MkX Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)
newtype Y = MkY Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)


data Rock = MkRock (Set (X,Y))

instance Semigroup Rock where
    MkRock s1 <> MkRock s2 = MkRock (s1 <> s2)

instance Monoid Rock where
    mempty = MkRock mempty

leftEdge :: Rock -> X
leftEdge (MkRock sp) = minimum (Set.map fst sp)

bottom :: Rock -> Y
bottom (MkRock sp) = minimum (Set.map snd sp)

height :: Rock -> Y
height (MkRock sp) 
    | Set.null sp = 0
    | otherwise   = Set.findMax (Set.map snd sp)

-- | Infinite list of rocks in the right order
shapes :: [Rock]
shapes = cycle [hline, plus, el, vline, square]
    where
        hline  = MkRock [ (0,0), (1,0), (2,0), (3,0) ]
        plus   = MkRock [ (0,0), (1,0), (0,1), (-1,0), (0,-1) ]
        el     = MkRock [ (0,0), (1,0), (2,0), (2,1), (2,2) ]
        vline  = MkRock [ (0,0), (0,1), (0,2), (0,3) ]
        square = MkRock [ (0,0), (0,1), (1,0), (1,1) ]


shift :: Jet -> Rock -> Rock
shift JLeft rock@(MkRock sp)
    | leftEdge rock == 0 = rock
    | otherwise          = MkRock $ Set.map (\(x,y) -> (x - 1, y)) sp
shift JRight rock@(MkRock sp)
    | (maximum (Set.map fst sp)) == 6 = rock
    | otherwise                       = MkRock $ Set.map (\(x,y) -> (x + 1, y)) sp


fall :: Rock -> Rock
fall (MkRock sp) = MkRock $ Set.map (\(x,y) -> (x, y - 1)) sp


overlaps :: Rock -> Rock -> Bool
overlaps (MkRock sp1) (MkRock sp2) 
    = not $ Set.null $ sp1 `Set.intersection` sp2





-- | Spawn a new rock, and let it fall until it cannot anymore.
letRockFall :: [Jet] 
            -> [Rock]  -- ^ List of rocks which will fall
            -> Rock    -- ^ Rocks fallen so far
            -> ([Jet], [Rock], Rock)
letRockFall []   _                  _        = error "No more jets"
letRockFall _    []                 _        = error "No more shapes"
letRockFall jets (shape:nextRocks) baseRock =
    let (newBaseRock, newJets) = go jets spawn baseRock
    in (newJets, nextRocks, newBaseRock)
    where
        -- Note that since the rocks all start with an origin of (0,0),
        -- we'll always need to shift them rightward and upwards at spawn.
        spawn :: Rock
        spawn = let haligned = head $ dropWhile (\rock -> leftEdge rock < 2)        
                                    $ iterate (shift JRight) shape
                 in head $ dropWhile (\rock -> bottom rock < (height baseRock) + 3 + 1) 
                         $ iterate (\(MkRock sp') -> MkRock $ Set.map (\(x,y) -> (x, y+1)) sp') haligned
        
        -- The go function includes two steps:
        -- 1. Jet pushing either left or right
        -- 2. Falling downwards if possible
        go :: [Jet] -> Rock -> Rock -> (Rock, [Jet])
        go [] _ _     = error "No more jets"
        go (_:[]) _ _ = error "Finite number of jets"
        go (jet:nextJets) rock tower 
            = case fallInContext $ shiftInContext rock of
                Left fallenRock  -> (tower <> fallenRock, nextJets)
                Right fallenRock -> go nextJets fallenRock tower
            where

                shiftInContext :: Rock -> Rock
                shiftInContext rock' 
                    = if tentative `overlaps` tower
                        then rock'
                        else tentative
                    where
                        tentative = shift jet rock'

                -- We use Left to signify that the rock cannot fall further, 
                -- and Right to signify that it can fall futher at the next step
                fallInContext :: Rock -> Either Rock Rock
                fallInContext rock' 
                    | tentative `overlaps` tower = Left rock'
                    | bottom tentative == 0      = Left rock'
                    | otherwise                  = Right tentative
                    where
                        tentative = fall rock'


simulateFall :: Int -> [Jet] -> Rock
simulateFall numRocks jets = run numRocks jets shapes mempty
    where

        run :: Int -> [Jet] -> [Rock] -> Rock -> Rock
        run 0 _ _ baseRock = baseRock
        run num jets' shapes' baseRock 
            = let (nextJets, nextShapes, newBaseRock) = letRockFall jets' shapes' baseRock 
               in run (num - 1) nextJets nextShapes newBaseRock