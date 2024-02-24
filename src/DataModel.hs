{-# LANGUAGE DeriveGeneric #-}

module DataModel where
    import Data.Aeson
    import GHC.Generics

    data Time = Time {
        hours :: Int,
        minutes :: Int,
        seconds :: Int,
        milliseconds :: Int
    } deriving (Generic, Show)

    data Date = Date {
        day :: Int,
        month :: Int,
        year :: Int
    } deriving (Generic, Show)

    data Measure = 
          MeasureRepetitions Int
        | MeasureTime String
        | MeasureDistance Float
        | MeasureCalories Int
        | MeasureWeight Float
        deriving (Generic, Show)

    data Iteration = 
        IterateByReps Int
      | IterateByDist Int
      deriving (Generic, Show)

    data Scalers = 
        ScaleDistance Float
      | ScaleWeight Float
      | ScaleIncreaseRoundReps Int
      | ScaleRPE (Int,Int)
      deriving (Generic, Show)

    data Movement = Movement {
        description :: String,
        notes :: String,
        labels :: [String],
        targets :: [String],
        iteration :: Iteration,
        scalers :: [String],
        measures :: [Measure],
        submovements :: [Movement]
    } deriving (Generic, Show)

    data BlockIteration =
          Amrap String
        | ForTime
        | ForTimeCap String
        | Sets Int
        deriving (Generic, Show)

    data BlockMeasure =
          MeasureBlockReps Int
        | MeasureBlockWeight Float
        | MeasureBlockDistance Float
        | NoBlockMeasure
        deriving (Generic, Show)

    data Block = Block {
        id :: Int,
        blockIteration :: BlockIteration,   
        blockMeasure :: BlockMeasure,
        blockNotes :: String,    
        movements :: [Movement]
    } deriving (Generic, Show)

    data TrainingDay = TrainingDay {
        date :: String,
        blocks :: [Block]
    } deriving (Generic, Show)

    instance ToJSON Measure where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON Iteration where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON Scalers where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON Movement where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON BlockIteration where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON BlockMeasure where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON Block where
        toEncoding = genericToEncoding defaultOptions
    instance ToJSON TrainingDay where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Measure
    instance FromJSON Iteration
    instance FromJSON Scalers
    instance FromJSON Movement
    instance FromJSON BlockIteration
    instance FromJSON BlockMeasure
    instance FromJSON Block
    instance FromJSON TrainingDay