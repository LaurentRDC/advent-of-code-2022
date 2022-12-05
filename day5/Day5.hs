{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import Control.Applicative  ( optional, Alternative(some, (<|>), many) )
import Data.Attoparsec.Text ( endOfInput, char, endOfLine, notChar, space, decimal, parseOnly, satisfy, string, Parser )
import Data.Char            ( isUpper ) 
import Data.Coerce          ( coerce )
import Data.IntMap          ( IntMap, (!) )
import qualified Data.IntMap as IntMap
import Data.List            ( transpose )
import Data.Maybe           ( catMaybes )
import Data.Monoid          ( Endo(..) )
import qualified Data.Text.IO as TIO

import System.Environment   ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            Input{..} <- readInput (parseInput <* endOfInput) inputFilePath

            putStrLn "[Part 1] letters of top crates:"
            -- Note that Endo f <> Endo g will be applied as f . g. Hence, we reverse the order of instructions
            let part1Computation = mconcat [Endo (runInstructionOneCrateAtATime i) | i <- reverse instructions]
            print $ message $ appEndo part1Computation crates

            putStrLn "[Part 2] letters of top crates:"
            -- Note that Endo f <> Endo g will be applied as f . g. Hence, we reverse the order of instructions
            let part2Computation = mconcat [Endo (runInstructionMultiCrate i) | i <- reverse instructions]
            print $ message $ appEndo part2Computation crates


newtype Crate = Crate Char 
    deriving Show


data Instruction 
    = Instruction { numCratesToMove :: Int
                  , sourcePos :: Int
                  , targetPos :: Int
                  }
    deriving Show

data Input = Input { crates       :: IntMap [Crate]
                   , instructions :: [Instruction]
                   }
    deriving (Show)


message :: IntMap [Crate] -> String
message = map (\(_, c) -> coerce (head c)) . IntMap.toAscList


runInstructionOneCrateAtATime :: Instruction -> IntMap [Crate] -> IntMap [Crate]
runInstructionOneCrateAtATime Instruction{..} stacks 

    = IntMap.insert sourcePos cratesStaying 
    -- Note that we're moving crates one by one, hence reversing cratesToMove
    $ IntMap.adjust (\l -> reverse cratesToMove <> l) targetPos stacks
    where
        cratesToMove  = take numCratesToMove $ stacks ! sourcePos
        cratesStaying = drop numCratesToMove $ stacks ! sourcePos


runInstructionMultiCrate :: Instruction -> IntMap [Crate] -> IntMap [Crate]
runInstructionMultiCrate Instruction{..} stacks 

    = IntMap.insert sourcePos cratesStaying 
    $ IntMap.adjust (\l -> cratesToMove <> l) targetPos stacks
    where
        cratesToMove  = take numCratesToMove $ stacks ! sourcePos
        cratesStaying = drop numCratesToMove $ stacks ! sourcePos


readInput :: Parser a -> FilePath -> IO a
readInput parser fp = do
    contents <- TIO.readFile fp
    either error pure (parseOnly parser contents)


parseCrate :: Parser (Maybe Crate)
parseCrate = parseCrate' <|> parseNoCrate
    where 
        parseCrate' = do
            c <- char '[' *> satisfy isUpper <* char ']'
            pure $ Just (Crate c)
        parseNoCrate = string "   " *> pure Nothing


parseCrateLines :: Parser (IntMap [Crate])
parseCrateLines = 
    -- Skipping over the indexing line at the bottom of the crates
    asStacks <$> some line <* (many (notChar '\n') <* endOfLine)
    where
        line :: Parser [Maybe Crate]
        line = do
            crates    <- some (parseCrate <* char ' ')
            lastCrate <- parseCrate <* endOfLine
            pure $ crates <> [lastCrate]
        
        asStacks :: [[Maybe Crate]] -> IntMap [Crate]
        asStacks crates = IntMap.fromDistinctAscList (zip [1..] (map (\c -> catMaybes c) (transpose crates)))


parseInstruction :: Parser Instruction
parseInstruction = do
    numCratesToMove <- string "move" *> space *> decimal <* space
    sourcePos <- string "from" *> space *> decimal <* space
    targetPos <- string "to" *> space *> decimal
    pure $ Instruction{..}


parseInput :: Parser Input
parseInput = do
    crates <- parseCrateLines
    endOfLine
    instructions <- some (parseInstruction <* optional endOfLine)
    pure $ Input{..}