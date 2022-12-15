{-# LANGUAGE NumericUnderscores#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import Control.Applicative    ( many )
import Data.Attoparsec.Text   ( endOfInput, decimal, signed, endOfLine, parseOnly, string, Parser )
import Data.List              ( find )
import Data.Maybe             ( fromJust )
import qualified Data.Set     as Set
import qualified Data.Text.IO as TIO
import System.Environment     ( getArgs )


main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            readings <- readInput (many (parseSensorReading <* endOfLine) <* endOfInput) inputFilePath
            let lineHeight = 2_000_000
            putStrLn $ "[Part 1] Number of locations at " <> show lineHeight <> " which isn't a beacon."
            print $ numPositionsWhereNoBeacons readings lineHeight

            putStrLn "[Part 2] Tuning frequency of the distress beacon"
            print $ tuningFreq $ findBeaconInLimits readings 4_000_000
            
    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)
        
        parseSensorReading :: Parser SensorReading
        parseSensorReading 
            = MkSensorReading <$> ( (,) <$> (string "Sensor at x=" *> (signed decimal))
                                        <*> (string ", y=" *> signed decimal)
                                  )
                              <*> ( (,) <$> (string ": closest beacon is at x=" *> signed decimal)
                                        <*> (string ", y=" *> signed decimal)
                                  )
        
        tuningFreq :: (X,Y) -> Int
        tuningFreq (x,y) = fromIntegral y + 4_000_000 * fromIntegral x 


newtype X = MkX Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)
newtype Y = MkY Int deriving (Eq, Ord, Enum, Show, Num, Real, Integral)

data SensorReading 
    = MkSensorReading { sensorPosition :: (X,Y)
                      , nearestBeacon  :: (X,Y)
                      } 
    deriving (Show)


distance :: (X,Y) -> (X,Y) -> Int
distance (x1, y1) (x2, y2) 
    = fromIntegral (abs (x1 - x2)) + fromIntegral (abs (y1 - y2))


numPositionsWhereNoBeacons :: [SensorReading] -> Y -> Int
numPositionsWhereNoBeacons readings y = length $ filter (\p -> closeToAnySensor p && notAlreadyABeaconThere p) coordsToCheck
    where
        maxDist = maximum $ (\(MkSensorReading p1 p2) -> distance p1 p2) <$> readings
        minx = (minimum $ fst . sensorPosition <$> readings) - fromIntegral maxDist
        maxx = (maximum $ fst . sensorPosition <$> readings) + fromIntegral maxDist
        coordsToCheck = (,y) <$> [minx .. maxx]

        closeToAnySensor :: (X,Y) -> Bool
        closeToAnySensor p = any (\MkSensorReading{..} -> distance sensorPosition p <= distance sensorPosition nearestBeacon) readings

        notAlreadyABeaconThere :: (X,Y) -> Bool
        notAlreadyABeaconThere = flip notElem (nearestBeacon <$> readings)


-- | Generating the outline of the region where we know the distress beacon cannot be
boundary :: Int -> SensorReading -> [(X,Y)]
boundary limit MkSensorReading{..}
    = filter inLimit $ concat [ [(sx + fromIntegral hOffset, sy + fromIntegral (dist - hOffset)) | hOffset <- [(-dist) .. dist]]
                              , [(sx + fromIntegral hOffset, sy + fromIntegral (hOffset - dist)) | hOffset <- [(-dist) .. dist]] 
                              ]
    where
        (sx, sy) = sensorPosition
        dist     = 1 + distance sensorPosition nearestBeacon

        inLimit (x, y) = all id [ x >= 0, x <= fromIntegral limit, y >= 0, y <= fromIntegral limit ]


findBeaconInLimits :: [SensorReading] 
                   -> Int -- ^ coordinate limit
                   -> (X,Y)
findBeaconInLimits readings limit 
    =  fromJust $ find (\p -> p `Set.notMember` beacons &&  not (tooCloseToSensor p)) candidates 
    where
        candidates = concat $ (boundary limit) <$> readings
        beacons = Set.fromList $ nearestBeacon <$> readings

        tooCloseToSensor p = any (\MkSensorReading{..} -> distance sensorPosition p <= distance sensorPosition nearestBeacon) readings