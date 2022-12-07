{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import Control.Applicative ( Alternative((<|>), many, some), optional )
import Control.Monad       ( forM_, void )
import Control.Monad.State.Strict ( modify, put, get, evalState, State )
import Control.Monad.Writer.Strict ( WriterT, tell, execWriterT )
import Data.Attoparsec.Text ( endOfInput, decimal, space, endOfLine, parseOnly, satisfy, string, Parser )
import Data.Char            ( isAlpha )
import Data.Either          ( partitionEithers )
import Data.List            ( unfoldr, inits, sortOn )
import Data.Map.Strict      ( Map, (!) )
import qualified Data.Map.Strict as Map
import Data.Set             ( Set )
import qualified Data.Set   as Set
import qualified Data.Text.IO as TIO
import System.Environment   ( getArgs )
import System.FilePath      ( (</>), dropTrailingPathSeparator, splitFileName )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            commands <- readInput (commandHistory <* endOfInput) inputFilePath
            let files = crawl commands
                sizes  = directorySizes files
            
            putStrLn "[part 1] Total size of directories with a size of at most 100000"
            print $ sum [val | (_, val) <- Map.toList sizes, val <= 100000]

            putStrLn "[part 2] Size of the directory that should be deleted"
            let unusedSpace = 70000000 - sizes ! "/"
                needToClear = 30000000 - unusedSpace
                dirToDelete = head $ sortOn ((!) sizes) [dir | dir <- Map.keys sizes, sizes ! dir > needToClear]
            print (Map.lookup dirToDelete sizes)
    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)


type FileSize = Int

data File a = MkFile FilePath a deriving Show


directorySizes :: [File FileSize] -> Map FilePath FileSize
directorySizes files = Map.fromList [ (dir, sizeOf dir) | dir <- Set.toList dirs ]
    where 
        dirs = foldl Set.union (Set.singleton "/") $ parents <$> files

        sizeOf :: FilePath -> FileSize
        sizeOf d = sum [ sz | MkFile _ sz <- relevantFiles ]
            where
                relevantFiles = filter (\(MkFile fp _) -> fp `isChildOf` d) files


isChildOf :: FilePath -> FilePath -> Bool
xs `isChildOf` ys = ys `elem` Set.fromList (dropTrailingPathSeparator <$> inits xs)


data CommandHistory
    = CD FilePath
    | LS [File FileSize]
    deriving Show


type Crawler a = WriterT [File FileSize] (State FilePath) a


runCrawler :: Crawler a -> [File FileSize]
runCrawler act = evalState (execWriterT act) "/"


parent :: FilePath -> FilePath
parent = dropTrailingPathSeparator . fst . splitFileName


parents :: File a -> Set FilePath
parents (MkFile fp _) = Set.fromList $ unfoldr (\f -> if f == "/" then Nothing else Just (f, parent f)) (parent fp)


processInstruction :: CommandHistory -> Crawler ()
processInstruction (CD fp)
    | fp == ".." = get >>= \cwd -> put (parent cwd)
    | otherwise  = modify (</> fp)
processInstruction (LS files) = do
    cwd <- get
    tell $ (\(MkFile fp x) -> MkFile (cwd </> fp) x) <$> files


crawl :: [CommandHistory] -> [File FileSize]
crawl instructions = runCrawler $ forM_ instructions processInstruction


commandHistory :: Parser [CommandHistory]
commandHistory = some (cdP <|> lsP <|> fail "Unknown command")
    where
        cdP = CD <$> (string "$ cd " *> some (satisfy (\c -> isAlpha c || c == '.' || c == '/') ) <* endOfLine)

        lsP = do
            void $ string "$ ls" <* endOfLine
            fs <- many (Left <$> dir <|> Right <$> file)
            let (_, files) = partitionEithers fs
            pure $ LS files
                
        dir = (string "dir" *> space *> some (satisfy isAlpha) <* optional endOfLine)
        
        file = do
            size     <- decimal <* space
            filename <- some (satisfy (\c -> isAlpha c || c == '.') ) <* optional endOfLine
            pure $ MkFile filename size

