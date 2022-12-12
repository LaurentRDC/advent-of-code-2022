{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
import Control.Applicative
import Data.Attoparsec.Text         ( endOfInput, endOfLine, parseOnly, satisfy, Parser )

import Data.Char                    ( isLower, ord )
import Data.Map.Strict              ( Map )
import qualified Data.Map.Strict    as Map

import qualified Data.Text.IO       as TIO       
import System.Environment           ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            inp <- readInput parseGrid inputFilePath
            putStrLn "[Part 1] Shortest path from S to E"
            print $ findShortestPathFromSToE inp

            putStrLn "[Part 2] Shortest path from any starting point to E"
            print $ findShortestPathFromAnyAToE inp

    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)


type Elevation = Int
type X = Int
type Y = Int

type Grid = Map (X, Y) Elevation

data Input = MkInput { grid          :: Grid
                     , startingPoint :: (X,Y)
                     , destination   :: (X,Y)
                     }
    deriving (Show)


parseGrid :: Parser Input
parseGrid = do
    rows <- some (some (satisfy (\c -> isLower c || c == 'E' || c == 'S' )) <* endOfLine) <* endOfInput
    let grid = buildGrid 0 rows mempty
        (grid', startingPoint) = findAndReplace (=='S') 'a' grid
        (grid'', destination) = findAndReplace (=='E') 'z' grid'
    pure $ MkInput (Map.map (\c -> ord c - ord 'a' + 1) grid'') startingPoint destination
    where 
        buildGrid :: Int -> [[Char]] -> Map (X,Y) Char -> Map (X,Y) Char
        buildGrid _ [] grid          = grid
        buildGrid rowIndex (nextRow:rest) grid 
            = buildGrid (rowIndex + 1) rest 
            $ foldl (\grid' (colIndex, c) -> Map.insert (rowIndex, colIndex) c grid') grid (zip [0..length nextRow - 1] nextRow)
        
        findAndReplace :: (Char -> Bool) -> Char -> Map (X,Y) Char -> (Map (X,Y) Char, (X,Y))
        findAndReplace predicate replacement grid = 
            let (startingPoint, _) = head $ Map.toList $ Map.filter predicate grid
             in (Map.insert startingPoint replacement grid, startingPoint)


type History = Map (X,Y) (X,Y)
type IsDone = (X,Y) -> Bool
type Direction = Int -> Int -> Bool 


-- | Breadth-first search algorithm
bfs :: Grid
    -> IsDone       -- ^ Function to tell us when we've arrived at the solution
    -> Direction    -- ^ Function which tells us whether it's valid to go to the next node
    -> [(X,Y)]      -- ^ Queue of nodes to visit
    -> History 
    -> ( Maybe (X,Y), History )
bfs _ _ _ [] history = (Nothing, history)
bfs grid isDone isValidDirection ((x,y):rest) history 
    | isDone (x,y)   = (Just (x,y), history)
    | otherwise      = bfs grid isDone isValidDirection nextQueue nextHistory
    where
        nextQueue = rest <> neighbors
        isValid (r, c) = all id [ (r, c) `Map.member` grid
                                , isValidDirection (grid Map.! (x,y)) (grid Map.! (r,c))
                                , (r,c) `Map.notMember` history
                                ]
        neighbors = filter isValid [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]
        nextHistory = foldl (\m k -> Map.insert k (x,y) m)  history neighbors


-- Build path from history of traversal
recover :: (X, Y) -> (Maybe (X, Y), History) -> [(X, Y)]
recover _ (Nothing, _)          = error "<unknown path>"
recover from (Just to, history) = reverse . takeWhile (/= from) . iterate (history Map.!) $ to


findShortestPathFromSToE :: Input -> Int
findShortestPathFromSToE MkInput{..} = 
    let x = bfs grid (==destination) goingUp [startingPoint] (Map.singleton startingPoint startingPoint)
    in length $ recover startingPoint x
    where
        goingUp :: Direction
        goingUp a b = b <= a + 1


-- Instead of looping over all possible starting points - some of which might not ever be connected to E -
-- we proceed DOWNWARDS from the endpoint point to the first point with lowest elevation.
findShortestPathFromAnyAToE :: Input -> Int
findShortestPathFromAnyAToE MkInput{..} = 
    let arrived n = (grid Map.! n) == 1
        x = bfs grid arrived goingDown [destination] (Map.singleton destination destination)
    in length $ recover destination x
    where
        goingDown :: Direction
        goingDown a b = a <= b + 1