
import           Control.Applicative  ( some, optional )

import           Data.Attoparsec.Text ( endOfInput, endOfLine, parseOnly, Parser, anyChar, skipSpace )
import qualified Data.Text.IO         as TIO

import           Numeric.Natural      ( Natural )

import           Prelude              hiding ( round )

import           System.Environment   ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            rounds <- readInputFirstPart inputFilePath
            putStrLn "[part 1] Total personal score:"
            print $ sum (score <$> rounds)

            inferredRounds <- readInputSecondPart inputFilePath
            putStrLn "[part 2] Total personal score from inferred shape:"
            print $ sum (score <$> inferredRounds)

data Shape 
    = Rock
    | Paper
    | Scissors
    deriving (Bounded, Enum, Show)


data Round 
    = MkRound { myShape       :: Shape
              , opponentShape :: Shape
              }
    deriving Show


data InferredRound
    = MkInferredRound { desiredOutcome :: Outcome
                      , irOpponentShape  :: Shape
                      }
    deriving Show


data Outcome
    = Loss
    | Draw
    | Win
    deriving (Eq, Show)


roundOutcome :: Round -> Outcome
roundOutcome (MkRound me op) = go me op
    where
        go :: Shape -> Shape -> Outcome
        go Rock Scissors  = Win
        go Scissors Rock  = Loss

        go Paper Rock     = Win
        go Rock Paper     = Loss

        go Paper Scissors = Loss
        go Scissors Paper = Win

        go _ _            = Draw


class HasScore a where
    score :: a -> Natural


instance HasScore Round where
    score round@(MkRound me _) = scoreFromOutcome + scoreFromShape
        where
            scoreFromOutcome = case roundOutcome round of
                Win  -> 6
                Draw -> 3
                Loss -> 0
            scoreFromShape = case me of
                Rock     -> 1
                Paper    -> 2
                Scissors -> 3

instance HasScore InferredRound where
    score (MkInferredRound desired op) = score (MkRound myShape op)
        where
            -- We've looking for the shape which will give us the right outcome.
            -- This isn't very efficient, I just wanted to save myself from writing more lines
            myShape = head 
                    $ filter (\myPotentialShape -> roundOutcome (MkRound myPotentialShape op) == desired) 
                             (enumFromTo minBound maxBound)


readInputFirstPart :: FilePath -> IO [Round]
readInputFirstPart fp = do
    contents <- TIO.readFile fp
    either error pure (parseOnly (roundsParser <* endOfInput) contents)


readInputSecondPart :: FilePath -> IO [InferredRound]
readInputSecondPart fp = do
    contents <- TIO.readFile fp
    either error pure (parseOnly (inferredRoundsParser <* endOfInput) contents)


roundsParser :: Parser [Round]
roundsParser = some (roundParser <* optional endOfLine)
    where 
        roundParser = do
            opponentShape <- parseOpponentShape
            skipSpace
            myShape       <- parseMyShape
            pure $ MkRound myShape opponentShape
        
        parseOpponentShape = do
            c <- anyChar
            case c of
                'A' -> pure Rock
                'B' -> pure Paper
                'C' -> pure Scissors
                _   -> fail "Unexpected character"

        parseMyShape = do
            c <- anyChar
            case c of
                'X' -> pure Rock
                'Y' -> pure Paper
                'Z' -> pure Scissors
                _   -> fail "Unexpected character"


inferredRoundsParser :: Parser [InferredRound]
inferredRoundsParser = some (inferredRoundParser <* optional endOfLine)
    where 
        inferredRoundParser = do
            opponentShape <- parseOpponentShape
            skipSpace
            desiredOutcome <- parseDesiredOutcome
            pure $ MkInferredRound desiredOutcome opponentShape
        
        parseOpponentShape = do
            c <- anyChar
            case c of
                'A' -> pure Rock
                'B' -> pure Paper
                'C' -> pure Scissors
                _   -> fail "Unexpected character"

        parseDesiredOutcome = do
            c <- anyChar
            case c of
                'X' -> pure Loss
                'Y' -> pure Draw
                'Z' -> pure Win
                _   -> fail "Unexpected character"
