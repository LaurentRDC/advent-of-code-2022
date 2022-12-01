import           Control.Applicative  ( some, optional )
import           Control.Monad        ( void )

import           Data.Attoparsec.Text ( endOfInput, decimal, endOfLine, parseOnly, Parser )
import           Data.List            ( sortOn )
import qualified Data.Text.IO         as TIO 

import           System.Environment   ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            elfs <- readInput inputFilePath

            -- Part 1 of the problem
            putStrLn "Maximum number of calories: "
            print (maximum $ totalCalories <$> elfs)
            let top3 = take 3 $ reverse $ sortOn totalCalories elfs

            -- Part 2 of the problem
            putStrLn "Total of the calories carried by the top 3 elves"
            print (sum (totalCalories <$> top3))


newtype ElfCalories = ElfCalories [Int]
    deriving (Eq, Show)


totalCalories :: ElfCalories -> Int
totalCalories (ElfCalories xs ) = sum xs


readInput :: FilePath -> IO [ElfCalories]
readInput fp = do
    contents <- TIO.readFile fp
    either error pure (parseOnly (some elfParser <* endOfInput) contents)


elfParser :: Parser ElfCalories
elfParser = do
    -- The last element of the input file is not followed by a newline nor by an empty line.
    -- Therefore, the both endOfLines are optional
    nums <- some (decimal <* optional endOfLine)
    void $ optional endOfLine    
    pure $ ElfCalories nums
