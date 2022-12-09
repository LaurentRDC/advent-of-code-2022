{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import Control.Applicative

import Control.Monad                  ( forM_ )
import Control.Monad.State.Strict     ( unless, when, StateT, gets, modify', evalStateT )     
import Control.Monad.Writer.Strict    ( MonadWriter(tell), execWriter, Writer )
import Data.Attoparsec.Text           ( choice, endOfInput, decimal, endOfLine, parseOnly, string, Parser )
import Data.Map.Strict                ( Map )
import qualified Data.Map.Strict      as Map
import Data.Set                       ( Set )
import qualified Data.Set             ( singleton )
import qualified Data.Text.IO as TIO
import System.Environment             ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            moves <- toSingleStepMoves <$> readInput ( some (parseMove <* endOfLine) <* endOfInput) inputFilePath
            let tailVisited = runSimulationPart1 $ mapM_ stepPart1 moves
            putStrLn "[Part 1] Number of unique positions visited by the rope tail:"
            print $ length $ tailVisited

            let tail9Visited = runSimulationPart2 $ mapM_ stepPart2 moves
            putStrLn "[Part 2] Number of unique positions visited by the ninth tail:"
            print $ length $ tail9Visited

    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)

        parseMove :: Parser MultiMove
        parseMove = choice [ MultUp    <$> (string "U " *> decimal)
                           , MultDown  <$> (string "D " *> decimal)
                           , MultLeft  <$> (string "L " *> decimal)
                           , MultRight <$> (string "R " *> decimal)
                           ]


data Coord = MkCoord Int Int
    deriving ( Eq, Ord )

add :: Coord -> Coord -> Coord
(MkCoord x y) `add` (MkCoord a b) = (MkCoord (x + a) (y + b))

instance Show Coord where
    show (MkCoord x y) = show (x,y)


distance2D :: Coord -> Coord -> (Int, Int)
distance2D (MkCoord x1 y1) (MkCoord x2 y2) = (x1-x2, y1-y2)


isAdjacent :: Coord -> Coord -> Bool
isAdjacent c1 c2 = case distance2D c1 c2 of
    (0, 1) -> True; (0, -1) -> True;
    (1, 0) -> True; (-1, 0) -> True;
    (1, 1) -> True; (-1, -1) -> True;
    (1, -1) -> True; (-1, 1) -> True;
    _ -> False


data MultiMove 
    = MultUp Int
    | MultDown Int
    | MultLeft Int
    | MultRight Int
    deriving Show

data Move = MUp | MDown | MLeft | MRight


toSingleStepMoves :: [MultiMove] -> [Move]
toSingleStepMoves = concatMap f
    where
        f (MultUp i) = replicate i MUp
        f (MultDown i) = replicate i MDown
        f (MultLeft i) = replicate i MLeft
        f (MultRight i) = replicate i MRight


data RopeState1Knot 
    = RS { headCoordinates :: Coord
         , tailCoordinates :: Coord
         }

type SimulationPart1 a = StateT RopeState1Knot (Writer (Set Coord)) a 

runSimulationPart1 :: SimulationPart1 () -> Set Coord
runSimulationPart1 sim = execWriter $ evalStateT sim' (RS (MkCoord 0 0) (MkCoord 0 0))
    where
        sim' = (tell $ Data.Set.singleton (MkCoord 0 0)) >> sim
    

stepPart1 :: Move -> SimulationPart1 ()
stepPart1 mv = moveHead >> moveTail
    where
        moveHead = do
            let (moveX, moveY) = case mv of
                    MUp    -> ( 0,  1)
                    MDown  -> ( 0, -1)
                    MLeft  -> (-1,  0)
                    MRight -> ( 1,  0)
            modify' (\st@(RS (MkCoord i j) _) -> st{headCoordinates = MkCoord (i + moveX) (j + moveY)})

        moveTail = do
            adjacent <- isAdjacent <$> gets tailCoordinates <*> gets headCoordinates
            unless adjacent $ do
                (MkCoord i j) <- gets tailCoordinates
                (h, v) <- distance2D <$> gets headCoordinates <*> gets tailCoordinates
                let newTailCoord =  MkCoord (i + signum h) (j + signum v)
                tell $ Data.Set.singleton newTailCoord
                modify' (\st -> st{tailCoordinates = newTailCoord})

-- Part 2
-- I know that part 1 is subsumed by some form of the following calculation, but I don't
-- want to take the time to refactor part 1.

data RopePart 
    = Head | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9
    deriving ( Eq, Ord, Enum, Bounded ) 

type SimulationPart2 a = StateT (Map RopePart Coord) (Writer (Set Coord)) a 


runSimulationPart2 :: SimulationPart2 () -> Set Coord
runSimulationPart2 sim 
    = execWriter $ evalStateT sim' $ Map.fromList [(part, (MkCoord 0 0)) | part <- enumFromTo minBound maxBound]
    where
        sim' = (tell $ Data.Set.singleton (MkCoord 0 0)) >> sim
    

stepPart2 :: Move -> SimulationPart2 ()
stepPart2 mv = do
    moveHead
    forM_ pairs $ \(leader, follower) -> movePart leader follower

    where
        parts = enumFromTo Head T9
        pairs = zip parts (tail parts)

        moveHead = do
            let move = case mv of
                    MUp    -> MkCoord 0 1
                    MDown  -> MkCoord 0 (-1)
                    MLeft  -> MkCoord (-1) 0
                    MRight -> MkCoord 1 0
            modify' (\map' -> Map.adjust (add move) Head map')

        movePart leader follower = do
            leaderCoords   <- gets $ \m -> m Map.! leader
            followerCoords <- gets $ \m -> m Map.! follower
            
            unless (isAdjacent leaderCoords followerCoords) $ do
                let (MkCoord i j) = followerCoords
                    (h, v) = distance2D leaderCoords followerCoords
                    newFollowerCoord =  MkCoord (i + signum h) (j + signum v)
                
                when (follower == T9) $
                    tell $ Data.Set.singleton newFollowerCoord

                modify' (\map' -> Map.insert follower newFollowerCoord map')