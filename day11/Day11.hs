{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

import Control.Applicative          ( some, optional, (<|>) )
import Control.Monad                ( void, replicateM_, forM_ )
import Control.Monad.State.Strict   ( gets, modify', runState, State )

import Data.Attoparsec.Text         ( endOfInput, decimal, skipSpace, space, char, endOfLine, parseOnly, string, Parser )
import Data.IntMap.Strict           ( IntMap )
import qualified Data.IntMap.Strict       as IntMap
import Data.List                    ( sort )
import Data.Monoid                  ( Sum(..) )
import Data.Sequence                ( Seq(..), (|>) )
import qualified Data.Sequence      as Seq
import qualified Data.Text.IO       as TIO

import System.Environment           ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            !monkeyState <- readInput (parseMonkeys <* endOfInput) inputFilePath
            let itemsInspectedPart1 = runInterpreter monkeyState $ replicateM_ 20 $ runRound (\w -> w `div` 3)
            
            putStrLn "[Part 1] Monkey business: "
            print $ product $ take 2 $ reverse $ sort $ IntMap.elems $ unCount itemsInspectedPart1

            -- Here's the problem: Running this simulation 10 000 times takes more memory than
            -- my computer can provide, given that the numbers become very large.
            -- However, there's a math trick we can employ. Note that each monkey is associated with
            -- a divisor, such that decisions about which monkey to throw items is based on the result of n `rem` divisor.
            -- 
            -- Note that all monkeys have prime divisors for both the test input and the full input. Therefore, we only
            -- care about the worriness of items up to `mod` (divisor1 * divisor2 * ... * divisorx). Modding all numbers
            -- by the product of all divisors, 'n', keeps worriness levels bounded to less than n.
            let n = product $ fmap divisor $ IntMap.elems monkeyState
                itemsInspectedPart2 = runInterpreter monkeyState $ replicateM_ 10_000 $ runRound (\w -> w `mod` n)
            print itemsInspectedPart2
            putStrLn "[Part 1] Monkey business: "
            print $ product $ take 2 $ reverse $ sort $ IntMap.elems $ unCount itemsInspectedPart2
            
    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)


type WorryLevel = Int
type MonkeyIndex = Int
newtype Operation = MkOperation { runOp :: WorryLevel -> WorryLevel }

data Monkey = MkMonkey { items     :: Seq WorryLevel
                       , operation :: Operation 
                       , divisor   :: WorryLevel
                       , ifTrue    :: MonkeyIndex
                       , ifFalse   :: MonkeyIndex
                       }

data Operator = Plus | Mult
    deriving Show


parseOperation :: Parser Operation
parseOperation = do
    skipSpace
    void $ string "Operation: new = old "
    op <- (char '*' *> pure Mult <|> char '+' *> pure Plus) <* space
    -- We use Nothing to represent the value 'old', and Just int otherwise
    operand <- (string "old" *> pure Nothing ) <|> (Just <$> decimal)
    case (op, operand) of 
        (Mult, Nothing) -> pure $ MkOperation $ \w -> w * w
        (Plus, Nothing) -> pure $ MkOperation $ \w -> w + w
        (Mult, Just i)  -> pure $ MkOperation $ \w -> w * i
        (Plus, Just i)  -> pure $ MkOperation $ \w -> w + i


parseMonkeys :: Parser (IntMap Monkey)
parseMonkeys = do
    mks <- some parseMonkey
    pure $ IntMap.fromAscList mks

    where
        parseMonkey = do
            i <- string "Monkey " *> decimal <* char ':' <* endOfLine
            items <- (Seq.fromList <$> parseItems)
            op <- parseOperation
            divisor <- skipSpace *> string "Test: divisible by "        *> decimal <* endOfLine
            ifTrue  <- skipSpace *> string "If true: throw to monkey "  *> decimal <* endOfLine
            ifFalse <- skipSpace *> string "If false: throw to monkey " *> decimal <* endOfLine
            void $ some endOfLine
            pure (i, MkMonkey items op divisor ifTrue ifFalse)
    
        parseItems = skipSpace *> string "Starting items: " 
                               *> some (decimal <* optional (string ", ")) 
                               <* endOfLine
            
newtype VisitorCounter = VC { unCount :: IntMap (Sum Int) }
    deriving newtype Show

instance Semigroup VisitorCounter where
    VC map1 <> VC map2 = VC $ IntMap.unionWith (<>) map1 map2

instance Monoid VisitorCounter where
    mempty = VC $ IntMap.empty


-- We're using the State monad as both a state and writer monad
-- because the WriterT from Control.Monad.Writer.Strict isn't really strict.
data InterpreterState = IS { monkeys        :: IntMap Monkey
                           , visitorCounter :: VisitorCounter 
                           }

type Interpreter a = State InterpreterState a


runInterpreter :: IntMap Monkey -> Interpreter a -> VisitorCounter
runInterpreter initState f = let (_, finalState) = runState f (IS initState mempty) 
                              in visitorCounter finalState

runRound :: (WorryLevel -> WorryLevel) -> Interpreter ()
runRound worryReduction = do
    ix <- gets (IntMap.keys . monkeys)
    forM_ ix $ \i -> do
        mk@MkMonkey{..} <- gets $ (IntMap.! i) . monkeys

        modify' (\s@IS{..} -> s{monkeys = IntMap.insert i mk{items=Seq.empty} monkeys}) 

        forM_ items $ \item -> do
            let worryLevel = worryReduction (runOp operation item)

            let targetMonkey = if worryLevel `rem` divisor == 0 then ifTrue else ifFalse 
            
            giveMonkeyItem targetMonkey worryLevel

        -- Each time a monkey inspects an item, we append the monkey index to a list
        -- Later, we'll group by the monkey index
        modify' (\s@IS{..} -> s{visitorCounter = visitorCounter <> (VC $ IntMap.singleton i (Sum $ length items))}) 
    
    where
        giveMonkeyItem :: MonkeyIndex -> WorryLevel -> Interpreter ()
        giveMonkeyItem ix item 
            = modify' $ \s@IS{..} -> s{monkeys=IntMap.adjust (\mk@MkMonkey{..} -> mk{items = items |> item}) ix monkeys}