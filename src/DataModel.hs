{-# LANGUAGE DeriveGeneric #-}

module DataModel where
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
        scalers :: [Scalers],
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
        date :: Date,
        blocks :: [Block]
    } deriving (Generic, Show)