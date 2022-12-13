import Control.Applicative    ( some, many, (<|>), optional )
import Control.Monad          ( void )

import Data.Attoparsec.Text   ( Parser, endOfInput, endOfLine, parseOnly, decimal, char ) 
import Data.List              ( elemIndex, sort )
import Data.Maybe             ( fromJust )
import qualified Data.Text.IO as TIO

import System.Environment     ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            pairs <- readInput (some parsePair <* endOfInput) inputFilePath

            putStrLn "[Part 1] Sum of indices of pairs in right order: "
            print $ sum . fmap fst . filter snd . zip [1..] . fmap rightOrder $ pairs

            let dividerPackets = [PList [PList [PInt 2]], PList [PList [PInt 6]]] 
                allPackets     = dividerPackets <> concatMap (\(p1,p2) -> [p1, p2]) pairs
                sortedPackets  = sort allPackets

                ix1 = elemIndexFrom1 (PList [PList [PInt 2]]) sortedPackets
                ix2 = elemIndexFrom1 (PList [PList [PInt 6]]) sortedPackets
            
            putStrLn "[Part 2] decoder key: "
            print $ ix1 * ix2
    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)

        parsePacket :: Parser Packet
        parsePacket = do
            void $ char '['
            ps <- many $ ((PInt <$> decimal) <|> parsePacket) <* optional (char ',')
            void $ char ']'
            pure $ PList ps

        parsePair :: Parser (Packet, Packet)
        parsePair = (,) <$> parsePacket <* endOfLine 
                        <*> parsePacket <* endOfLine <* endOfLine

        elemIndexFrom1 :: Eq a => a -> [a] -> Int
        elemIndexFrom1 needle haystack = (fromJust $ elemIndex needle haystack) + 1


data Packet = PInt Int 
            | PList [Packet]
    deriving (Eq)

instance Ord Packet where
    compare (PInt i) (PInt j)             = compare i j
    compare (PInt i) (PList p)            = compare (PList [PInt i]) (PList p)
    compare (PList p) (PInt i)            = compare (PList p) (PList [PInt i])
    -- Note that the Monoid instance of Ord is short-circuiting
    -- https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Base.html#line-415
    compare (PList (x:xs)) (PList (y:ys)) = compare x y <> compare (PList xs) (PList ys)
    compare (PList []) (PList [])         = EQ
    compare (PList []) (PList _)          = LT
    compare (PList _) (PList [])          = GT


rightOrder :: (Packet, Packet) -> Bool
rightOrder (xs, ys) = compare xs ys /= GT

