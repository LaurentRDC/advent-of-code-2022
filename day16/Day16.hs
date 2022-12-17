{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative  ( optional, Alternative(many) )
import Data.Attoparsec.Text ( choice, endOfInput, decimal, endOfLine, parseOnly, satisfy, string, Parser )
import Data.Char            ( isUpper )
import Data.Foldable        ( foldl' )
import Data.List            ( (\\) )
import qualified Data.List as List
import Data.Map.Strict     ( Map )
import qualified Data.Map.Strict  as Map
import Data.Monoid         ( Sum(..) )
import qualified Data.Text.IO as TIO
import System.Environment  ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            -- The implementation below is rather verbose and not tidy at all.
            -- I don't have time right now to clean it up
            (graph, flowRates) <- readInput (parseConnections <* endOfInput) inputFilePath
            let dists = distances flowRates graph
                candidates = paths 30 dists flowRates
            
            let times = openingTime 30 dists <$> candidates
            let mostPressureReleased = maximum $ fmap (pressureReleased 30 flowRates) times
            putStrLn "[Part 1] maximum pressure released: "
            print $ mostPressureReleased

            let candidatesPart2 = paths 26 dists flowRates
            let disjointPairs = [(p1, p2) | p1 <- candidatesPart2, p2 <- candidatesPart2, isDisjoint p1 p2]

            let mostPressureReleasedPart2 = maximum [ pressureReleased 26 flowRates (openingTime 26 dists p1) 
                                                    + pressureReleased 26 flowRates (openingTime 26 dists p2) 
                                                    | (p1, p2) <- disjointPairs 
                                                    ]
            putStrLn "[Part 2] Most pressure released with help from an elephant:"
            print mostPressureReleasedPart2

            
    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)


type ValveGraph = Map ValveName [ValveName]

newtype ValveName = MkValveName (Char, Char) deriving (Eq)

instance Ord ValveName where
    -- Note that the Monoid instance of Ord short-circuits
    compare (MkValveName (a,b)) (MkValveName (c,d)) = compare a c <> compare b d

instance Show ValveName where
    show (MkValveName (c,d)) = show [c,d]

type FlowRates = Map ValveName Int


parseConnections :: Parser (ValveGraph, FlowRates)
parseConnections = do
    ls <- many (parseLine <* endOfLine)
    let flowRates = Map.fromList [(name, flowRate) | (name, flowRate, _) <- ls]

    pure $ (buildGraph [ (name, conns) | (name, _, conns) <- ls ], flowRates)

    where 
        parseName :: Parser ValveName
        parseName = MkValveName <$> ((,) <$> satisfy isUpper <*> satisfy isUpper)

        parseLine :: Parser ( ValveName, Int, [ValveName])
        parseLine = (,,) <$> (string "Valve " *> parseName)
                         <*> (string " has flow rate=" *> decimal)
                         <*> choice [ string "; tunnels lead to valves " *> many (parseName <* optional (string ", "))
                                    -- thanks for this edge case ಠ_ಠ
                                    , string "; tunnel leads to valve "  *> (List.singleton <$> parseName)
                                    ]
        
        buildGraph :: [(ValveName, [ValveName])] -> ValveGraph
        buildGraph = go mempty
            where
                go :: ValveGraph -> [(ValveName, [ValveName])] -> ValveGraph
                go graph [] = graph
                go graph ((name, conns):xs) = go (Map.insertWith mappend name conns graph ) xs


type RunningTime = Int
type OpeningTimes = Map ValveName Int

pressureReleased :: Int -> FlowRates -> OpeningTimes -> Int
pressureReleased limit flowRates openings 
    -- Note the +1 constant because the last period (the 30th minute) is inclusive
    = getSum $ Map.foldMapWithKey (\k t -> Sum $ (limit - t + 1) * (flowRates Map.! k)) openings


type Path = [ValveName]

paths :: Int -> Distances -> FlowRates -> [ Path ]
paths limit dists flowRates = List.nub $ fmap fst $ go [ ([startingValve], 0) ]
    where
        startingValve = MkValveName ('A', 'A')
        workingValves = Map.keys $ Map.filter (>0) flowRates

        go :: [ (Path, Int) ] -> [ (Path, Int) ]
        go = nTimes (length workingValves) (concatMap f)
            where
                f :: (Path, Int) -> [ (Path, Int) ]
                f (p, i)
                    | i > limit = [ (p, i) ]
                    | otherwise = [ (p, i) ] ++ [ (p <> [n], i + dists Map.! (last p, n) + 1) 
                                                | n <- workingValves \\ p
                                                ]

isDisjoint :: Path -> Path -> Bool
isDisjoint (MkValveName ('A', 'A'):p1) (MkValveName ('A', 'A'):p2) = List.null (List.intersect p1 p2)
isDisjoint _ _ = error "Paths don't start with AA"

-- | Apply a function @n@ times to a given value.
nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f


openingTime :: Int -> Distances -> [ValveName] -> OpeningTimes
openingTime limit dists (MkValveName ('A', 'A'):rest) = go (MkValveName ('A', 'A')) rest 1 mempty
    where
        go :: ValveName -> [ValveName] -> RunningTime -> OpeningTimes -> OpeningTimes
        go _    []          _  openings = openings
        go from (next:rest') rt openings
            | rt > limit = openings
            | otherwise = let nsteps   = dists Map.! (from, next)
                              openedBy = rt + nsteps + 1
                          in if openedBy >= limit
                            then openings
                            else go next rest' openedBy (Map.insert next openedBy openings)
openingTime _ _ path = error $ "Path does not start with AA: " <> show path


-- | Breadth-first search algorithm
bfs :: ValveGraph
    -> (ValveName -> Bool)       -- ^ Function to tell us when we've arrived at the solution
    -> [ValveName]  -- ^ Queue of nodes to visit
    -> Map ValveName ValveName 
    -> ( Maybe ValveName, Map ValveName ValveName )
bfs _ _ [] history = (Nothing, history)
bfs graph isDone (vlv:rest) history 
    | isDone vlv     = (Just vlv, history)
    | otherwise      = bfs graph isDone nextQueue nextHistory
    where
        nextQueue   = rest <> neighbors
        neighbors   = filter (\v -> v `Map.notMember` history) $ graph Map.! vlv
        nextHistory = foldl' (\m k -> Map.insert k vlv m) history neighbors


type Distances = Map (ValveName, ValveName) Int


distances :: FlowRates -> ValveGraph -> Distances
distances flowRates graph = Map.fromList [ ((v1, v2), shortestPathLength v1 v2) | v1 <- functionalValves, v2 <- functionalValves ]
    where
        startingValve = MkValveName ('A', 'A')
        functionalValves = [startingValve] <> (Map.keys $ Map.filter (\flowRate -> flowRate > 0) flowRates)

        shortestPathLength :: ValveName -> ValveName -> Int
        shortestPathLength from to 
            = let x = bfs graph (==to) [from] (Map.singleton from from)
            in length $ recover from x
            where
                -- Build path from history of traversal
                recover :: ValveName -> (Maybe ValveName, Map ValveName ValveName) -> [ValveName]
                recover _ (Nothing, _)          = error "<unknown path>"
                recover from' (Just to', history) = reverse . List.takeWhile (/= from') . iterate (history Map.!) $ to'