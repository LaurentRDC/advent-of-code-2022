{-# LANGUAGE RecordWildCards #-}
import Control.Applicative          ( many )

import Data.Attoparsec.Text         as P ( endOfInput, endOfLine, parseOnly, takeWhile, Parser )
import qualified Data.List          as List 
import Data.Map.Strict              ( Map )
import qualified Data.Map.Strict    as Map
import Data.Set                     ( Set )
import qualified Data.Set           as Set
import Data.Text                    ( Text )
import qualified Data.Text          as Text
import qualified Data.Text.IO       as TIO

import System.Environment           ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            elves <- findElves <$> readInput (parseGrid <* endOfInput) inputFilePath
            
            putStrLn "[Part 1] Number of free squares:"
            print $ part1 elves
            putStrLn "[Part 2] Number of rounds until steady state:"
            print $ part2 elves


    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)
        
        parseGrid :: Parser [Text]
        parseGrid = many $ P.takeWhile (\c -> c `elem` [ '.', '#' ]) <* endOfLine

        findElves :: [Text] -> Set (X,Y)
        findElves ls = go 0 ls mempty
            where
                go :: Y -> [Text] -> Set (X,Y) -> Set (X,Y)
                go _ []   elves = elves
                go row (hd:rest) elves = let newElves = Set.fromList $ map (\(col,_) -> (col, row)) 
                                                                     $ filter (\(_, c) -> c == '#') 
                                                                     $ zip [0::X ..] 
                                                                     $ Text.unpack hd
                                          in go (row + 1) rest (elves `Set.union` newElves)


newtype X = MkX Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)
newtype Y = MkY Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

data Direction = North | South | East | West
type Directions = (Direction, Direction, Direction, Direction)

directions :: [ Directions ]
directions = cycle [ (North, South, East, West)
                   , (South, East, West, North)
                   , (East, West, North, South)
                   , (West, North, South, East) 
                   ]


move :: Direction -> (X,Y) -> (X,Y)
move North (x,y) = (x, y-1)
move South (x,y) = (x, y+1)
move East  (x,y) = (x-1, y)
move West  (x,y) = (x+1, y)


blocksInDirection :: Direction -> (X,Y) -> Set (X,Y)
blocksInDirection North (x,y) = Set.fromList [ (x-1, y-1), (x, y-1), (x+1, y-1) ]
blocksInDirection South (x,y) = Set.fromList [ (x-1, y+1), (x, y+1), (x+1, y+1) ]
blocksInDirection East  (x,y) = Set.fromList $ (x-1,) <$> [ y-1, y, y+1 ]
blocksInDirection West  (x,y) = Set.fromList $ (x+1,) <$> [ y-1, y, y+1 ]

proposal :: Directions -> Set (X,Y) -> (X,Y) -> (X,Y)
proposal (p1, p2, p3, p4) allElves elf
    -- Special case: do nothing
    | Set.null $ allElves `Set.intersection` surrounding elf          = elf

    | Set.null $ allElves `Set.intersection` blocksInDirection p1 elf = move p1 elf
    | Set.null $ allElves `Set.intersection` blocksInDirection p2 elf = move p2 elf
    | Set.null $ allElves `Set.intersection` blocksInDirection p3 elf = move p3 elf
    | Set.null $ allElves `Set.intersection` blocksInDirection p4 elf = move p4 elf
    | otherwise                                                       = elf
    where
        surrounding :: (X,Y) -> Set (X,Y)
        surrounding (x,y) = Set.fromList [ (x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], (x', y') /= (x,y) ]


count :: Ord a => [a] -> Map a Int
count xs = Map.fromList [ (e, length $ filter (==e) xs) | e <- xs ] 


data State = MkState { sElves      :: Set (X,Y)
                     , sDirections :: [Directions]
                     }

transition :: State -> State
transition MkState{..} = MkState { sElves      = movedElves
                                 , sDirections = tail sDirections
                                 }
    where
        proposals = Map.fromList [(elf, proposal (head sDirections) sElves elf) | elf <- Set.toList sElves]
        validProposals = Set.fromList $ map fst $ Map.assocs $ Map.filter (<2) $ count $ Map.elems proposals


        movedElves = Set.map mv sElves
            where
                mv elf = let proposed = proposals Map.! elf 
                          in if proposed `Set.member` validProposals then proposed else elf


part1 :: Set (X,Y) -> Int
part1 elves = ( width * height  ) - length elves
    where
        MkState{..} = last $ take (10+1) $ iterate transition (MkState elves directions)
        minx = fromIntegral $ Set.findMin $ Set.map fst sElves
        maxx = fromIntegral $ Set.findMax $ Set.map fst sElves
        miny = fromIntegral $ Set.findMin $ Set.map snd sElves
        maxy = fromIntegral $ Set.findMax $ Set.map snd sElves
        width  = maxx - minx + 1
        height = maxy - miny + 1


part2 :: Set (X,Y) -> Int
-- Adding a +2 constant because the first round and last rounds count
part2 elves = 2 + (fst $ last $ go 0 (MkState elves directions) mempty)
    where
        go :: Int -> State -> [ (Int, State) ] -> [ (Int, State) ]
        go ix state progress = let state' = transition state
                                in if sElves state' == sElves state
                                 then progress
                                 else go (ix+1) state' $ progress <> [(ix, state')]



draw :: Set (X,Y) -> String
draw elves = List.intercalate "\n" $ go 0 []
    where
        
        go :: Y -> [String] -> [ String ]
        go row ls
            | row > height = ls
            | otherwise    = let (newline::String) = [ if (col,row) `Set.member` shiftedElves then '#' else '.' | col <- [0..width] ]
                              in go (row+1) $ ls <> [newline]

        minx = Set.findMin $ Set.map fst elves
        maxx = Set.findMax $ Set.map fst elves
        width = maxx - minx
        miny = Set.findMin $ Set.map snd elves
        maxy = Set.findMax $ Set.map snd elves
        height = maxy - miny

        shiftedElves = Set.map ( \(x,y) -> (x - minx, y - miny)) elves