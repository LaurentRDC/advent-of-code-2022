{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import Control.Applicative            ( Alternative(some) ) 

import Data.Attoparsec.Text           ( endOfInput, digit, endOfLine, parseOnly, Parser )
import Data.Char                      ( digitToInt )
import Data.Coerce                    ( coerce )
import qualified Data.List            ( transpose )
import qualified Data.Text.IO as TIO
import System.Environment             ( getArgs )
import Data.Vector                    ( fromList, toList, Vector )
import qualified Data.Vector          as Vector


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            gridOfRows <- readInput (parseGrid <* endOfInput) inputFilePath
            let gridOfCols = transposeRows gridOfRows
                visibleL = flatten $ visibleFromLeft gridOfRows
                visibleR = flatten $ visibleFromRight gridOfRows
                visibleT = flatten $ visibleFromTop gridOfCols
                visibleB = flatten $ visibleFromBottom gridOfCols 
                visible = Vector.zipWith4 isVisible visibleL visibleR visibleT visibleB
                numVisible = Vector.length $ Vector.filter id visible
            putStrLn "[Part 1] Number of trees visible from the outside:"
            print numVisible

            let rowScores = mapGrid rowScore gridOfRows
                colScores = mapGrid colScore gridOfCols
                totalScores = Vector.zipWith (*) (flatten rowScores) (flatten colScores)

            putStrLn "[Part 2] Maximum scenic score:"
            print (Vector.maximum totalScores)

    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)

        parseGrid :: Parser (Grid Row Height)
        parseGrid = MkGrid . fromList <$>  some (parseRow <* endOfLine)
            where
                parseRow = MkRow . fromList . fmap (fromIntegral . digitToInt) <$> some digit
        
        isVisible a b c d = a || b || c || d


newtype Grid m a = MkGrid (Vector (m a))
    deriving (Show)

mapGrid :: (m a -> m b) -> Grid m a -> Grid m b
mapGrid f (MkGrid xs) = MkGrid (f <$> xs)

newtype Row a = MkRow (Vector a) deriving (Show, Functor)
newtype Col a = MkCol (Vector a) deriving (Show, Functor)

newtype Height = MkHeight Int
    deriving (Show, Eq, Ord, Bounded, Enum, Num, Real, Integral)

type Scanner = (Height -> Height -> Height) -> Height -> Vector Height -> Vector Height

visibleH :: Scanner -> Grid Row Height -> Grid Row Bool
visibleH scanner (MkGrid rs) = MkGrid $ fmap (coerce . f . coerce) rs
    where
        f :: Vector Height -> Vector Bool
        f vs = let runningMax = scanner max (-1) vs
                in Vector.zipWith (>) vs runningMax

visibleV :: Scanner -> Grid Col Height -> Grid Col Bool
visibleV scanner (MkGrid cs) = MkGrid $ fmap (coerce . f . coerce) cs
    where
        f :: Vector Height -> Vector Bool
        f vs = let runningMax = scanner max (-1) vs
                in Vector.zipWith (>) vs runningMax

visibleFromLeft, visibleFromRight :: Grid Row Height -> Grid Row Bool
visibleFromLeft = visibleH Vector.prescanl'
visibleFromRight = visibleH Vector.prescanr'

visibleFromTop, visibleFromBottom :: Grid Col Height -> Grid Col Bool
-- Because of transposition rules, the bottom of a column becomes the right
visibleFromTop = visibleV Vector.prescanl'
visibleFromBottom = visibleV Vector.prescanr'


-- | flatten allows to flatten a grid into a single flat list of integers,
-- such that we are looking at elements row-by-row.
class Flatten m a b where
    flatten :: m a b -> Vector b

instance Flatten Grid Row b where
    flatten (MkGrid rows ) = mconcat $ (\(MkRow v) -> v) <$> toList rows

instance Flatten Grid Col b where
    flatten = flatten . transposeCols


transposeRows :: Grid Row a -> Grid Col a
transposeRows (MkGrid rows) = MkGrid (fromList (MkCol . fromList <$> cols))
    where
        -- I'm too lazy to write a transpose function on vectors
        lists = (\(MkRow v) -> toList v) <$> toList rows
        cols = Data.List.transpose lists

transposeCols :: Grid Col a -> Grid Row a
transposeCols (MkGrid cols) = MkGrid (fromList (MkRow . fromList <$> rows))
    where
        -- I'm too lazy to write a transpose function on vectors
        lists = (\(MkCol v) -> toList v) <$> toList cols
        rows = Data.List.transpose lists


-- Part 2

newtype Score = MkScore Int 
    deriving (Show, Eq, Ord, Enum, Bounded, Num, Real, Integral)

type Index = Int


rowScore :: Row Height -> Row Score
rowScore (MkRow rs) = MkRow $ Vector.zipWith (*) (scoreFromLeft rs) 
                                                 (Vector.reverse $ scoreFromLeft (Vector.reverse rs))
    where
        scoreFromLeft :: Vector Height -> Vector Score
        scoreFromLeft heights = Vector.imap (score heights) heights


colScore :: Col Height -> Col Score
colScore (MkCol rs) 
    = MkCol $ Vector.zipWith (*) (scoreFromTop rs) 
                                 (Vector.reverse $ scoreFromTop (Vector.reverse rs))
    where
        scoreFromTop :: Vector Height -> Vector Score
        scoreFromTop heights = Vector.imap (score heights) heights


-- | This function counts the number of trees which, starting at a particular index,
-- are smaller or equal to the height of the tree at the index, from LEFT to RIGHT.
score :: Vector Height 
      -> Index 
      -> Height 
      -> Score
score heights ix height
    = let relevant = Vector.drop 1 $ snd $ Vector.splitAt (coerce ix) heights
      in  coerce $ count height relevant
    where 
        -- Count the number of elements whose heights are less than `height`.
        -- Break AFTER the first tree which has a height greater or equal to `height`.
        -- For example, 
        --      count 5 [1,2,3,4,5,5] should return 5
        --      count 5 [1,2,3,4,9,5] should return 5
        count :: Height -> Vector Height -> Int
        count h xs = go 0 (Vector.toList xs)
            where
                go :: Int -> [Height] -> Int
                go acc [] = acc
                go acc (hd:rest)
                    | hd < h = go (acc + 1) rest
                    | hd >= h = acc + 1
                go _ _ = error "Impossible"