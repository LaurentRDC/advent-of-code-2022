{-#LANGUAGE OverloadedStrings #-}
import Control.Applicative   ( some, (<|>) )
import Data.Attoparsec.Text  ( endOfInput, decimal, signed, endOfLine, parseOnly, string, Parser )
import Data.IntMap.Strict    ( IntMap )
import Data.List             ( intercalate )
import Data.Monoid           ( Sum(..) )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text.IO as TIO
import System.Environment    ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            instructions <- oneInstructionPerCycle <$> readInput ( some (parseInstruction <* endOfLine) <* endOfInput) inputFilePath
            let registerValues = registerValue instructions
                sigStrength = signalStrengh registerValues

            putStrLn "[Part 1] Signal strength:"
            print sigStrength

            putStrLn "[Part 2] Visible sprite:"
            putStrLn $ drawScreen registerValues 

    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)

        oneInstructionPerCycle :: [Instruction] -> [Instruction]
        oneInstructionPerCycle = concatMap f
            where 
                f NoOp = [NoOp]
                f (Add i) = [NoOp, Add i]

        parseInstruction :: Parser Instruction
        parseInstruction = noop <|> addx
            where
                noop = string "noop" >> pure NoOp

                addx = Add <$> (string "addx " *> signed decimal)

data Instruction 
    = NoOp
    | Add Int
    deriving Show


type RegisterValue = Int

registerValue :: [Instruction] -> IntMap RegisterValue
registerValue = IntMap.fromAscList . zip [1..] . scanl go 1
    where
        go :: RegisterValue -> Instruction -> RegisterValue
        go acc (NoOp)  = acc
        go acc (Add i) = acc + i 

signalStrengh :: IntMap RegisterValue -> Int
signalStrengh m = getSum $ IntMap.foldMapWithKey go m
    where
        interestingCycles = [20, 60, 100, 140, 180, 220]

        go :: Int -> RegisterValue -> Sum Int
        go k v
            | k `elem` interestingCycles = Sum $ k * v
            | otherwise                  = mempty


type Cycle = Int

drawScreen :: IntMap RegisterValue -> String
drawScreen = intercalate "\n" 
           . toLinesOf40Char 
           . fmap go 
           . IntMap.toAscList
           . IntMap.mapKeys (\k -> k - 1) -- From cycle (1..40) to position (0 .. 39)
    where
        go :: (Cycle, RegisterValue) -> Char
        go (c, v) = let position = c `mod` 40
                     in if position `elem` [v-1, v, v+1] then '#' else ' '

        toLinesOf40Char :: [Char] -> [String]
        toLinesOf40Char xs = [ drop (n * 40) $ take ((n+1) * 40) xs | n <- [0..5]]