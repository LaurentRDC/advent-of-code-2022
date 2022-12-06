{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Data.Attoparsec.Text   ( Parser, parseOnly, endOfInput, takeText )
import qualified Data.Set     as Set
import Data.Text              ( Text )
import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO

import System.Environment   ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            buffer <- readInput (takeText <* endOfInput) inputFilePath
            putStrLn "[Part 1] Finding the start-of-packet marker position:"
            print $ findMarker 4 buffer

            putStrLn "[Part 2] Finding the start-of-message marker position:"
            print $ findMarker 14 buffer


type Buffer = Text
type Chunk = Text
type ChunkSize = Int


chunks :: ChunkSize -> Buffer -> [Chunk]
chunks size txt = go [] txt
    where 
        go :: [Chunk] -> Buffer -> [Chunk]
        go cks txt'
            | Text.null txt' = cks 
            | otherwise      = let newChunk = Text.take size txt'
                                in go (cks <> [newChunk]) (Text.drop 1 txt')


noRepeatedLetter :: Chunk -> Bool
noRepeatedLetter ck 
    = length (Set.fromList (Text.group ck)) == Text.length ck


findMarker :: ChunkSize -> Buffer -> Int
findMarker size buffer = go size (chunks size buffer)
    where
        go :: Int -> [Chunk] -> Int
        go acc [] = acc
        go acc (hd:rest)
            | noRepeatedLetter hd = acc
            | otherwise           = go (acc + 1) rest 


readInput :: Parser a -> FilePath -> IO a
readInput parser fp = do
    contents <- TIO.readFile fp
    either error pure (parseOnly parser contents)


