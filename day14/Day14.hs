{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
import Control.Applicative      ( some, optional )
import Control.Monad            ( void )
import Control.Monad.RWS.Strict ( asks, tell, modify', MonadReader(ask), MonadState(get), runRWS, RWS )

import Data.Attoparsec.Text     ( Parser, endOfInput, endOfLine, parseOnly, decimal, char, string ) 
import Data.Monoid              ( Sum(..) )
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
            rocks <- readInput (parseRockFormation <* endOfInput) inputFilePath
            let lowPoint = maximum (Set.map snd rocks)

            let part1Env = MkEnv { finalCondition = \(_, y) _ -> y >= lowPoint
                                 , isOnFloor      = const False 
                                 }

            putStrLn "[Part 1] Number of sand units at rest until they fall in void: "
            print $ runSimulation rocks part1Env simulate

            let floorHeight = 2 + maximum (Set.map snd rocks)
            let part2Env = MkEnv { finalCondition = \pt move -> pt == (MkX 500, MkY 0) && move == AtRest
                                 , isOnFloor      = \(_, y)  -> y + 1 == floorHeight 
                                 }

            putStrLn "[Part 2] Number of sand units at rest until source is blocked: "
            -- We're adding one to the count because the grain of sand
            -- at the source will not be counted. This keeps the simulation
            -- simpler
            print $ (runSimulation rocks part2Env simulate) + 1



    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)

        parseRockFormation :: Parser RockFormation
        parseRockFormation = mconcat <$> some (line <* endOfLine)
            where
                point :: Parser (X,Y)
                point = (,) <$> (MkX <$> decimal) <* char ',' <*> (MkY <$> decimal)

                line :: Parser RockFormation
                line = do
                    segments <- some (point <* optional (void $ string " -> "))
                    let pairs = zip segments (tail segments)
                    pure $ foldMap go pairs
                    where
                        go :: ((X,Y), (X,Y)) -> RockFormation
                        go ((x1, y1), (x2, y2))
                            | x1 == x2 = Set.fromList $ (x1,) <$> [min y1 y2 .. max y1 y2]
                            | y1 == y2 = Set.fromList $ (,y1) <$> [min x1 x2 .. max x1 x2]
                            | otherwise = error $ mconcat [ "Unexpected: line is neither vertical nor horizontal: from "
                                                          , show (x1, y1), " to ", show (x2, y2) 
                                                          ]


newtype X = MkX Int deriving (Eq, Ord, Enum, Show, Num)
newtype Y = MkY Int deriving (Eq, Ord, Enum, Show, Num)

type Rock = (X,Y)
type Sand = (X,Y)

type RockFormation = Set Rock

type FinalCondition = (X, Y) -> Move -> Bool 

data Env = MkEnv { finalCondition :: FinalCondition
                 , isOnFloor      :: (X, Y) -> Bool
                 }

type Simulation a = RWS Env (Sum Int) (Set (X,Y)) a


runSimulation :: RockFormation -> Env -> Simulation a -> Int
runSimulation rockFormation env f = let (_, _, w) = (runRWS f env rockFormation) in getSum w


simulate :: Simulation ()
simulate = do
    result <- simulateSandUnit
    case result of
        Done -> pure ()
        Continue -> simulate


data Result = Continue | Done


simulateSandUnit :: Simulation Result
simulateSandUnit = go (MkX 500, MkY 0)
    where
        go :: (X,Y) -> Simulation Result
        go (x, y) = do
            move <- allowedMove (x,y)
            doneCondition <- asks finalCondition
            if doneCondition (x,y) move
                then pure Done
                else case move of
                    Down        -> go (x, y + MkY 1)
                    DiagLeft    -> go (x - MkX 1, y + MkY 1)
                    DiagRight   -> go (x + MkX 1, y + MkY 1)
                    AtRest      -> do
                        modify' $ Set.insert (x,y)
                        tell (Sum 1)
                        pure Continue


allowedMove :: Sand -> Simulation Move
allowedMove (x,y) = do
    MkEnv{..} <- ask
    if isOnFloor (x,y)
        then pure $ AtRest
        else do
            limits <- get
            pure $ if not $ (x, y + MkY 1) `Set.member` limits
                then Down
                else if not $ (x - MkX 1, y + 1) `Set.member` limits
                    then DiagLeft
                    else if not $ (x + 1, y + 1) `Set.member` limits
                        then DiagRight
                        else AtRest


data Move = Down
          | DiagLeft
          | DiagRight
          | AtRest
    deriving (Eq)
