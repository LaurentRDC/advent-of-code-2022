
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            pairs <- readInput (parsePair <* optional endOfLine) inputFilePath
            putStrLn "[Part 1] Number of fully-overlapping pairs:"
            print $ length $ filter fullyOverlaps pairs

            putStrLn "[Part 2] Number of partially-overlapping pairs:"
            print $ length $ filter partiallyOverlaps pairs


data Range = MkRange Int Int
    deriving Show


data Pair = MkPair Range Range
    deriving Show


fullyOverlaps :: Pair -> Bool
fullyOverlaps (MkPair (MkRange from1 to1) (MkRange from2 to2)) 
    = ((from1 <= from2) && (to2 <= to1)) || ((from2 <= from1) && (to1 <= to2))


partiallyOverlaps :: Pair -> Bool
partiallyOverlaps (MkPair (MkRange from1 to1) (MkRange from2 to2)) 
    = let s1 = Set.fromList [from1 .. to1]
          s2 = Set.fromList [from2 .. to2]
      in not $ Set.null $ Set.intersection s1 s2 --(from1 <= from2 && from2 <= to1) || (from1 <= to2 && to2 <= to1)


readInput :: Parser a -> FilePath -> IO [a]
readInput parser fp = do
    contents <- TIO.readFile fp
    either error pure (parseOnly (many parser <* endOfInput) contents)


parsePair :: Parser Pair
parsePair = MkPair <$> parseRange <* char ','
                   <*> parseRange
    where
        parseRange = MkRange <$> decimal <* char '-'
                             <*> decimal