{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ( many )

import Data.Attoparsec.Text   ( choice, endOfInput, decimal, signed, endOfLine, parseOnly, string, Parser )
import qualified Data.Attoparsec.Text as Parser

import Data.Map.Strict        ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe             ( mapMaybe)
import Data.String            ( IsString )
import Data.Text              ( Text )
import qualified Data.Text.IO as TIO

import Prelude                hiding ( (!!) ) 

import System.Environment ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            jobs <- Map.fromList <$> readInput (many (parseJob <* endOfLine) <* endOfInput) inputFilePath
            putStrLn "[Part 1] Root's number: "
            print $ Map.lookup "root" $ evaluated $ solveRiddle jobs


    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)
        
        parseJob :: Parser (Monkey, Job)
        parseJob = (,) <$> ( monkey <* string ": ")
                       <*> choice [ Number <$> signed decimal
                                  , Add <$> monkey <* string " + " <*> monkey
                                  , Sub <$> monkey <* string " - " <*> monkey
                                  , Mul <$> monkey <* string " * " <*> monkey
                                  , Div <$> monkey <* string " / " <*> monkey
                                  ]
            where
                monkey = MkMonkey <$> Parser.take 4

newtype Monkey = MkMonkey Text
    deriving (Show, Eq, Ord, IsString)

data Job = Number Int
         | Add Monkey Monkey
         | Sub Monkey Monkey
         | Mul Monkey Monkey
         | Div Monkey Monkey
    deriving (Show, Eq)

data State 
    = MkState { evaluated     :: Map Monkey Int
              , remainingJobs :: Map Monkey Job
              }
    deriving (Eq)


initialState :: Map Monkey Job -> State
initialState = MkState mempty


transition :: State -> State
transition (MkState evaluated jobs) = MkState (Map.union (Map.fromList evs) evaluated) newJobs

    where
        evs = mapMaybe go $ Map.toList jobs
        newJobs = foldr (\(mk,_) mp -> Map.delete mk mp) jobs evs

        go :: (Monkey, Job) -> Maybe (Monkey, Int)
        go (mk, jb) = case runJob jb of
            Nothing -> Nothing
            Just i  -> Just $ (mk, i)

        runJobWith :: (Int -> Int -> Int) 
                   -> Monkey 
                   -> Monkey 
                   -> Maybe Int
        runJobWith f m1 m2 = do
            (v1, v2) <- (,) <$> Map.lookup m1 evaluated <*> Map.lookup m2 evaluated
            pure $ f v1 v2

        runJob :: Job -> Maybe Int
        runJob (Number i) = Just i
        runJob (Add m1 m2) = runJobWith (+) m1 m2
        runJob (Sub m1 m2) = runJobWith (-) m1 m2
        runJob (Mul m1 m2) = runJobWith (*) m1 m2
        runJob (Div m1 m2) = runJobWith div m1 m2


solveRiddle :: Map Monkey Job -> State
solveRiddle jobs = go (initialState jobs)
    where
        go :: State -> State
        go st  = let st' = transition st
                  in if st == st' 
                    then st' 
                    else go st'