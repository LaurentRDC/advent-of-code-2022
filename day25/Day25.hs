{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData      #-}
import Control.Applicative          ( many )

import Data.Attoparsec.Text         ( choice, endOfInput, char, endOfLine, parseOnly, Parser )
import Data.Coerce                  ( coerce )
import Data.Functor                 ( ($>) )

import qualified Data.Text.IO       as TIO

import System.Environment           ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            ns <- readInput (many (parseEncrypted <* endOfLine) <* endOfInput) inputFilePath
            putStrLn "[Part 1] SNAFU number to provide: "
            print $ toSnafu $ encrypt $ sum $ decrypt <$> ns

    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)
        
        parseEncrypted :: Parser Encrypted
        parseEncrypted = MkEncrypted . reverse <$> many encryptedDigit
            where
                encryptedDigit = choice [ char '2' $> 2 
                                        , char '1' $> 1
                                        , char '0' $> 0
                                        , char '-' $> -1
                                        , char '=' $> -2
                                        ]


newtype Encrypted = MkEncrypted [Int]
    deriving Show

newtype Base5 = MkBase5 [Int]
    deriving Show





decrypt :: Encrypted -> Int
decrypt = sum . zipWith (*) bases . coerce
    where
        bases = fmap (5^) [0::Int ..]


encrypt :: Int -> Encrypted
encrypt i = MkEncrypted $ map (\d -> d - 2) digits
    where
        b5@(MkBase5 xs) = toBase5 i
        offset = MkBase5 $ replicate (length xs + 1) 2
        (MkBase5 digits) = b5 |+| offset 

        (|+|) :: Base5 -> Base5 -> Base5
        (|+|) x1 x2 = toBase5 $ fromBase5 x1 + fromBase5 x2


toBase5 :: Int -> Base5
toBase5 i = MkBase5 $ go i []
    where
        go :: Int -> [Int] -> [Int]
        go x xs
            | q == 0    = xs <> [r]
            | otherwise = go q $ xs <> [r]
            where 
                (q, r) = x `quotRem` 5


fromBase5 :: Base5 -> Int
fromBase5 = sum . zipWith (*) bases . coerce
    where
        bases = fmap (5^) [0::Int ..]


toSnafu :: Encrypted -> String
toSnafu = reverse . map go . coerce
    where
        go :: Int -> Char
        go (-2) = '='
        go (-1) = '-'
        go   0  = '0'
        go   1  = '1'
        go   2  = '2'
        go   n  = error $ "Impossible number: " <> show n
