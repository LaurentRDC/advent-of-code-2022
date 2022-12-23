{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.IntMap.Strict           ( IntMap )
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe                   ( catMaybes, isJust, fromJust  )
import qualified Data.Text.IO       as TIO
import Numeric.Natural              ( Natural )

import System.Environment           ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []                -> putStrLn "Expecting a path to an input file"
        (inputFilePath:_) -> do
            blueprints <- readInput (parseBlueprints <* endOfInput) inputFilePath
            print $ IntMap.foldlWithKey' (\acc i bp -> acc + (i * fromIntegral (runBlueprint bp))) 0 blueprints -- (\(i,bp) acc -> acc + (fromIntegral i * fromIntegral $ runBlueprint bp)) 0 (IntMap.toList blueprints)

    where
        readInput :: Parser a -> FilePath -> IO a
        readInput parser fp = do
            contents <- TIO.readFile fp
            either error pure (parseOnly parser contents)
        
        parseBlueprints :: Parser (IntMap Blueprint)
        parseBlueprints = IntMap.fromList <$> (many (readBlueprint <* endOfLine))

        readBlueprint :: Parser (Int, Blueprint)
        readBlueprint = do
            i <- string "Blueprint " *> decimal <* string ": "
            oreRobotOreReq <- string "Each ore robot costs " *> decimal <* " ore. "
            clayRobotOreReq <- string "Each clay robot costs " *> decimal <* " ore. "
            obsidianRobotOreReq <- string "Each obsidian robot costs " *> decimal
            obsidianRobotClayReq <- string " ore and " *> decimal <* string " clay. "
            geodeRobotOreReq <- string "Each geode robot costs " *> decimal
            geodeRobotObsidianReq <- string " ore and " *> decimal <* string " obsidian."
            pure (i, MkBluePrint{..})


data Resource = Ore | Clay | Obsidian | Geode
    deriving (Eq, Ord, Show)


data Ore
data Clay
data Obsidian
data Geode
newtype AmountOf a = AmountOf Natural deriving (Eq, Ord, Enum, Show, Num, Real, Integral)


data Blueprint 
    = MkBluePrint { oreRobotOreReq        :: AmountOf Ore
                  , clayRobotOreReq       :: AmountOf Ore
                  , obsidianRobotOreReq   :: AmountOf Ore
                  , obsidianRobotClayReq  :: AmountOf Clay
                  , geodeRobotOreReq      :: AmountOf Ore
                  , geodeRobotObsidianReq :: AmountOf Obsidian
                  }
    deriving (Eq, Show)


data Resources = MkResources { ore :: AmountOf Ore
                             , clay :: AmountOf Clay
                             , obsidian :: AmountOf Obsidian
                             , geode :: AmountOf Geode
                             }
    deriving (Show, Eq)

data State 
    = MkState { oreRobots      :: Natural
              , clayRobots     :: Natural 
              , obsidianRobots :: Natural
              , geodeRobots    :: Natural
              , resources      :: Resources
              , prevResources  :: Resources
              , blueprint      :: Blueprint
              }
    deriving (Eq, Show)


initialState :: Blueprint -> State
initialState = MkState 1 0 0 0 noResources noResources
    where noResources = MkResources 0 0 0 0


canBuildOreRobot :: Blueprint -> Resources -> Bool
canBuildOreRobot MkBluePrint{..} MkResources{..} = oreRobotOreReq <= ore

canBuildClayRobot :: Blueprint -> Resources -> Bool
canBuildClayRobot MkBluePrint{..} MkResources{..} = clayRobotOreReq <= ore

canBuildObsidianRobot :: Blueprint -> Resources -> Bool
canBuildObsidianRobot MkBluePrint{..} MkResources{..} = obsidianRobotOreReq <= ore && obsidianRobotClayReq <= clay

canBuildGeodeRobot :: Blueprint -> Resources -> Bool
canBuildGeodeRobot MkBluePrint{..} MkResources{..} = geodeRobotOreReq <= ore && geodeRobotObsidianReq <= obsidian


transition :: State -> [State]
transition state@MkState{..}
    | isJust buildGeodeRobot    = [fromJust $ buildGeodeRobot]
    | otherwise                 = [state{resources=newResources, prevResources=resources}] <> catMaybes [buildObsidianRobot, buildClayRobot, buildOreRobot]
    where
        ensure :: Bool -> Maybe ()
        ensure  = guard

        newResources = let MkResources{..} = resources
                        in resources{ ore      = ore      + fromIntegral oreRobots
                                    , clay     = clay     + fromIntegral clayRobots
                                    , obsidian = obsidian + fromIntegral obsidianRobots
                                    , geode    = geode    + fromIntegral geodeRobots
                                    }

        buildOreRobot :: Maybe State
        buildOreRobot 
            = if not (canBuildOreRobot blueprint resources)
                then Nothing
                else do
                    let MkBluePrint{..} = blueprint
                    -- There's no point in building more ore robots than the maximum cost for one
                    -- since we can only ever build one robot per turn.
                    ensure $ fromIntegral oreRobots < maximum [ oreRobotOreReq, clayRobotOreReq, obsidianRobotOreReq, geodeRobotOreReq ]

                    pure $ state { oreRobots     = oreRobots + 1
                                 , resources     = newResources{ore = ore newResources - oreRobotOreReq }
                                 , prevResources = resources
                                 }

        buildClayRobot :: Maybe State
        buildClayRobot 
            = if not $ canBuildClayRobot blueprint resources
                then Nothing 
                else do
                    let MkBluePrint{..} = blueprint
                    -- There's no point in building more clay robots than the maximum cost for one
                    -- since we can only ever build one robot per turn.
                    ensure $ fromIntegral clayRobots < obsidianRobotClayReq

                    pure $ state { clayRobots    = clayRobots + 1
                                 , resources     = newResources{ore = ore newResources - clayRobotOreReq}
                                 , prevResources = resources
                                 }

        buildObsidianRobot :: Maybe State
        buildObsidianRobot 
            = if not $ canBuildObsidianRobot blueprint resources
                then Nothing
                else do
                    let MkBluePrint{..} = blueprint
                    -- There's no point in building more obsidian robots than the maximum cost for one
                    -- since we can only ever build one robot per turn.
                    ensure $ fromIntegral obsidianRobots < geodeRobotObsidianReq

                    pure $ state { obsidianRobots = obsidianRobots + 1
                                , resources = newResources { ore  = ore newResources  - obsidianRobotOreReq
                                                           , clay = clay newResources - obsidianRobotClayReq 
                                                           }
                                , prevResources = resources
                                }

        buildGeodeRobot :: Maybe State
        buildGeodeRobot 
            = if not $ canBuildGeodeRobot blueprint resources && not (canBuildGeodeRobot blueprint prevResources)
                then Nothing
                else do
                    pure $ state { geodeRobots = geodeRobots + 1
                                 , resources = newResources { ore      = ore newResources      - geodeRobotOreReq blueprint
                                                            , obsidian = obsidian newResources - geodeRobotObsidianReq blueprint
                                                            }
                                 , prevResources = resources
                                 }


runBlueprint :: Blueprint -> AmountOf Geode
runBlueprint bp = most geode $ go 24 [initialState bp] 
    where
        most :: (Resources -> AmountOf a) -> [State] -> AmountOf a
        most get states 
            | null states = 0
            | otherwise   = maximum $ fmap (get . resources) states
        

        go :: Int -> [State] -> [State]
        go 0 states     = states
        go nstep states = let newstates  = concat $ transition <$> states
                              maxObs     = most obsidian newstates
                              maxGeodes  = most geode newstates
                              newstates' = filter (\s -> geode (resources s) == maxGeodes || obsidian (resources s) == maxObs ) newstates
                           in go (nstep - 1) newstates'
