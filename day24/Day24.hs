{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
import Control.Applicative
import Control.Monad                ( guard )

import Data.Attoparsec.Text         as P

import Data.Hashable                ( Hashable )
import Data.HashSet                 ( HashSet )
import qualified Data.HashSet       as HashSet
import Data.Text                    ( Text )
import qualified Data.Text          as Text
import qualified Data.Text.IO       as TIO

import GHC.Generics                 ( Generic )

import System.Environment           ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            ls <- readInput (parseGrid <* endOfInput) inputFilePath
            let blizzards = findBlizzards ls
            -- Minus 2 because we're excluding the walls
            let gridWidth  = fromIntegral (Text.length $ head ls) - 2
                gridHeight = fromIntegral $ length ls - 2
                gridDims   = (gridWidth, gridHeight)
            
            putStrLn "[Part 1] Number of minutes: "
            let start = (0,-1)
                dest  = (gridWidth-1, gridHeight)
                (numMinutes, _) =  traverseField gridDims start dest blizzards
            print $ numMinutes

            putStrLn "[Part 2] Number of minutes: "
            let (numMinutes1, bs1) =  traverseField gridDims start dest blizzards
                (numMinutes2, bs2) =  traverseField gridDims dest start bs1
                (numMinutes3, _)   =  traverseField gridDims start dest bs2
            print $ numMinutes1 + numMinutes2 + numMinutes3
    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)

        parseGrid :: Parser [Text]
        parseGrid = many $ P.takeWhile (\c -> c `elem` [ '.', '#', '^', 'v', '>', '<']) <* endOfLine
        
        findBlizzards :: [Text] -> HashSet Blizzard
        findBlizzards ls = go (-1) ls mempty
            where
                go :: Y -> [Text] -> HashSet Blizzard -> HashSet Blizzard
                go _ []              bs = bs
                go row (hd:rest) elves 
                    = let newBs = HashSet.fromList $ map (\(col,dir) -> MkBlizzard (col, row) (mkDir dir)) 
                                                   $ filter (\(_, c) -> c `elem` ['<', '>', '^', 'v']) 
                                                   $ zip [-1::X ..] 
                                                   $ Text.unpack hd
                       in go (row + 1) rest (elves `HashSet.union` newBs)
                
                mkDir 'v' = South
                mkDir '^' = North
                mkDir '<' = West
                mkDir '>' = East
                mkDir d   = error $ "Unknown direction: " <> show d

 


newtype X = MkX Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Hashable)
newtype Y = MkY Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral, Hashable)

data Direction = North | South | East | West
    deriving (Eq, Ord, Generic)
instance Hashable Direction

data Blizzard = MkBlizzard { position  :: !(X,Y) 
                           , direction :: !Direction
                           }
    deriving (Eq, Ord, Generic)
instance Hashable Blizzard


moveBlizzards :: (X,Y) -- ^ Width and height
              -> HashSet Blizzard
              -> HashSet Blizzard
moveBlizzards (width, height) bs = HashSet.map (\MkBlizzard{..} -> MkBlizzard (mv position direction) direction) bs
    where
        mv :: (X,Y) -> Direction -> (X,Y)
        mv (x,y) South = (x, (y+1) `mod` height)
        mv (x,y) North = (x, (y-1) `mod` height)
        mv (x,y) East  = ((x+1) `mod` width, y)
        mv (x,y) West  = ((x-1) `mod` width, y)


transition :: (X,Y)             -- ^ Grid dimensions
           -> (X,Y)             -- ^ Starting point
           -> (X,Y)             -- ^ Destination
           -> HashSet (X,Y)     -- ^ Where blizzards will be in the next minute
           -> (X,Y)             -- ^ Where the expedition is right now
           -> HashSet (X,Y)     -- ^ Possible locations where the expedition will be in the next minute
transition (width, height) start destination blizzardPositionsNext (x,y) 
    =  HashSet.fromList $ do
        next@(nx, ny) <- [(x,y), (x+1, y), (x-1, y), (x, y+1), (x,y-1)]
        guard $ next == destination || next == start || (nx >= 0 && nx < width && ny >= 0 && ny < height)
        guard $ not $ next `HashSet.member` blizzardPositionsNext
        pure next


traverseField :: (X,Y) -- ^ Grid dimensions
              -> (X,Y) -- ^ Starting point
              -> (X,Y) -- ^ Destination
              -> HashSet Blizzard
              -> (Int, HashSet Blizzard)
traverseField (width, height) start end blizzards = go 0 blizzards $ HashSet.fromList [start]
    where
        go :: Int -> HashSet Blizzard -> HashSet (X,Y) -> (Int, HashSet Blizzard)
        go !i bs xs 
            | HashSet.null xs         = error "No more ways to advance"
            | end `HashSet.member` xs = (i, bs)
            | otherwise               = let nextPts = foldMap (transition (width, height) start end blizzardPositionsNext) xs
                                         in go (i+1) blizzardsNext nextPts
            where
                blizzardsNext = moveBlizzards (width, height) bs
                blizzardPositionsNext = HashSet.map position blizzardsNext