import Control.Applicative          ( many )
import Data.Attoparsec.Text         ( endOfInput, decimal, signed, endOfLine, parseOnly, Parser )

import Data.Maybe                   ( fromJust )
import qualified Data.Text.IO       as TIO
import Data.Vector                  ( Vector)
import qualified Data.Vector        as Vector

import Prelude                      hiding ( (!!) )
import System.Environment           ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            xs <- readInput (parseList <* endOfInput) inputFilePath
            let indexedPart1 = Vector.indexed xs
                unmixedPart1 = fmap snd $ unmix indexedPart1
                        
            putStrLn "[Part 1] Decrypted: "
            print $ decrypted unmixedPart1

            let indexedPart2 = Vector.indexed $ fmap (*811589153) xs

            putStrLn "[Part 2] Decrypted: "
            print $ decrypted $ fmap snd $ last $ take (10+1) $ iterate unmix indexedPart2

    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)
        
        parseList :: Parser (Vector Int)
        parseList = Vector.fromList <$> many ( signed decimal <* endOfLine )

        decrypted :: Vector Int -> Int
        decrypted xs = let zeroIx = fromJust  $ Vector.findIndex (==0) xs
                        in sum [ xs Vector.! ((zeroIx + 1000) `mod` length xs)
                               , xs Vector.! ((zeroIx + 2000) `mod` length xs)
                               , xs Vector.! ((zeroIx + 3000) `mod` length xs)
                               ]


type Index = Int
type Order = Int


unmix :: Vector (Order, Int) -> Vector (Order, Int)
unmix es = go 0 es
    where
        go :: Order -> Vector (Order, Int) -> Vector (Order, Int)
        go order xs 
            | order == length xs = xs
            | otherwise = let elemix = fromJust $ Vector.findIndex (\(i, _) -> i == order) xs
                              (_, value) = xs Vector.! (elemix `mod` length xs) 
                           in go (order + 1) $ shift xs elemix (elemix + value)


shift :: Vector a 
      -> Index   -- ^ Shift element from this index
      -> Index   -- ^ to this index
      -> Vector a
shift xs from to 
    | to < 0    = let newTo = length xs + (to `mod` (length xs - 1)) - 1 
                   in shift xs from newTo
    | otherwise = insert (delete xs from) (to) (xs Vector.! (from `mod` length xs))


delete :: Vector a -> Index -> Vector a
delete xs ix 
    | ix < 0         = delete xs (length xs - abs ix)
    | ix > length xs = delete xs (ix `mod` length xs)
    | otherwise      = fmap snd $ Vector.filter (\(i,_) -> i /= ix) $ Vector.indexed xs


insert :: Vector a -> Index -> a -> Vector a
insert xs ix value 
    | ix < 0         = insert xs (length xs - abs ix) value
    | ix > length xs = insert xs (ix `mod` length xs) value
    | otherwise      = mconcat [ Vector.take ix xs, Vector.singleton value, Vector.drop ix xs ]