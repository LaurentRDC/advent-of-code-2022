
import Control.Applicative     ( optional, some, many )      

import Data.Attoparsec.Text    ( endOfInput, endOfLine, satisfy, parseOnly, Parser )
import Data.Char               ( ord, isLetter )
import Data.Monoid             ( Sum(..) )
import qualified Data.Set      as Set                
import qualified Data.Text.IO  as TIO

import System.Environment      ( getArgs )       

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            rucksacks <- readInput (rucksackParser <* optional endOfLine) inputFilePath
            let commonItems = commonItemType <$> rucksacks 
                totalPriority = foldMap ( Sum . priority ) commonItems
            putStrLn "[Part 1] Total: "
            print (getSum totalPriority)

            elfGroups <- readInput (elfGroupParser <* optional endOfLine) inputFilePath
            let badges = badgeItem <$> elfGroups
            putStrLn "[Part 2] Total: "
            print (getSum $ foldMap ( Sum . priority ) badges)


type Item = Char

-- | Should be: a = 1, b = 2, ..., z = 26
--              A = 27, ...        Z = 52
priority :: Item -> Int
priority c 
    | c `elem` ['a' .. 'z'] = ord c - ord 'a' + 1
    | c `elem` ['A' .. 'Z'] = ord c - ord 'A' + 1 + 26
    | otherwise             = 0


data RuckSack 
    = MkRuckSack [Item] [Item]
    deriving (Show)


data ElfGroup = MkElfGroup RuckSack RuckSack RuckSack 
    deriving Show


badgeItem :: ElfGroup -> Item
badgeItem (MkElfGroup (MkRuckSack c1 c2) (MkRuckSack c3 c4) (MkRuckSack c5 c6) ) 
    = let s1 = Set.fromList $ mconcat [c1, c2]
          s2 = Set.fromList $ mconcat [c3, c4]
          s3 = Set.fromList $ mconcat [c5, c6]
        in head $ Set.toList $ foldr1 Set.intersection [s1, s2, s3]


commonItemType :: RuckSack -> Item
commonItemType (MkRuckSack c1 c2) 
    = head $ Set.toList $ Set.intersection (Set.fromList c1) (Set.fromList c2)


readInput :: Parser a -> FilePath -> IO [a]
readInput parser fp = do
    contents <- TIO.readFile fp
    either error pure (parseOnly (many parser <* endOfInput) contents)


rucksackParser :: Parser RuckSack
rucksackParser = do
    items <- some $ satisfy isLetter
    let numItemsInCompartment = length items `div` 2
    pure $ MkRuckSack (take numItemsInCompartment items) (drop numItemsInCompartment items)


elfGroupParser :: Parser ElfGroup
elfGroupParser = MkElfGroup <$> rucksackParser <* endOfLine
                            <*> rucksackParser <* endOfLine
                            <*> rucksackParser