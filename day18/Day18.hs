{-# LANGUAGE RecordWildCards    #-}
import Control.Applicative      (  many )
                 
import Data.Attoparsec.Text     ( Parser, endOfInput, endOfLine, parseOnly, char, decimal )
import Data.Monoid              ( Sum(..) )
import Data.Set                 ( Set )
import qualified Data.Set       as Set
import qualified Data.Text.IO   as TIO

import System.Environment       ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            droplet <- Set.fromList <$> readInput ((many (parsePoint <* endOfLine)) <* endOfInput) inputFilePath
            putStrLn "[Part 1] Surface area of the droplet, including enclosed air pockets: "
            print $ surfaceArea droplet

            let negativeSpace = air droplet
                -- Because the negative space includes air outside the drop,
                -- the point which has minimal x,y,z coordinates will NOT be air inside of the droplet
                startingPoint = Set.findMin negativeSpace
                inside = expanding (Set.delete startingPoint negativeSpace) [startingPoint]

            putStrLn "[Part 2] Surface area of the droplet, excluding enclosed air pockets: "
            print $ surfaceArea (droplet `Set.union` inside)
            

   
    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)
        
        parsePoint :: Parser Point
        parsePoint = MkPoint <$> decimal <* char ','
                             <*> decimal <* char ','
                             <*> decimal


newtype X = MkX Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)
newtype Y = MkY Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)
newtype Z = MkZ Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

data Point = MkPoint { px :: X, py :: Y, pz :: Z }
    deriving (Show, Eq, Ord)


surfaceArea :: Set Point -> Int
surfaceArea pts = getSum $ foldMap (\pt -> surfaceAreaPoint (Set.delete pt pts) pt) pts
    where
        surfaceAreaPoint :: Set Point -> Point -> Sum Int
        surfaceAreaPoint pts' MkPoint{..} = foldMap (\pt -> if pt `Set.member` pts' then Sum 0 else Sum 1) neighbors
            where
                neighbors = [ MkPoint (px + 1) py pz, MkPoint (px-1) py pz
                            , MkPoint px (py + 1) pz, MkPoint px (py - 1) pz
                            , MkPoint px py (pz + 1), MkPoint px py (pz - 1)
                            ]

-- | Set if points which are NOT lava. This includes both air inside the lava drop and outside of it.
air :: Set Point -> Set Point
air pts = Set.filter (`Set.notMember` pts) candidates
    where
        (xs, ys, zs) = unzip3 $ Set.elems $ Set.map (\MkPoint{..} -> (px, py, pz)) pts
        candidates   = Set.fromList [ MkPoint x y z 
                                    | x <- [minimum xs .. maximum xs]
                                    , y <- [minimum ys .. maximum ys]
                                    , z <- [minimum zs .. maximum zs]
                                    ]


-- | Expanding a set of points to its maximum number of members
-- The final set contains points from the input which could NOT be reached
-- from the points in the list
expanding :: Set Point -> [Point] -> Set Point
expanding pts [] = pts
expanding pts (MkPoint{..}:rest) = expanding pts' rest'
    where
        neighbors = filter (`Set.member` pts) [ MkPoint (px + 1) py pz, MkPoint (px-1) py pz
                                              , MkPoint px (py + 1) pz, MkPoint px (py - 1) pz
                                              , MkPoint px py (pz + 1), MkPoint px py (pz - 1)
                                              ]
        pts'  = foldl (flip Set.delete) pts neighbors
        rest' = rest ++ neighbors
