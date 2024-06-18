{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module DataModel where
    import GHC.Generics
    import Data.Aeson
    
    data Time = Time {
        hours :: Integer,
        minutes :: Integer,
        seconds :: Integer,
        milliseconds :: Integer
    } deriving (Generic, Show)

    instance ToJSON Time
    instance FromJSON Time

    data Date = Date {
        day :: Integer,
        month :: Integer,
        year :: Integer
    } deriving (Generic, Show)

    instance ToJSON Date
    instance FromJSON Date

    data Measure = 
          MeasureRepetitions Integer
        | MeasureTime Time
        | MeasureDistance Integer
        | MeasureCalories Integer
        | MeasureWeight Double
        deriving (Generic, Show)

    instance ToJSON Measure
    instance FromJSON Measure

    data Iteration = 
        IterateByReps Integer
      | IterateByDist Integer
      deriving (Generic, Show)

    instance ToJSON Iteration
    instance FromJSON Iteration

    data Scalers = 
        ScaleDistance Integer
      | ScaleWeight Double
      | ScaleIncreaseRoundReps Integer
      | ScaleRPE (Integer,Integer)
      deriving (Generic, Show)

    instance ToJSON Scalers
    instance FromJSON Scalers

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

    instance ToJSON Movement
    instance FromJSON Movement

    data MovementParams =
          DescriptionParam String
        | NotesParam String
        | LabelsParam [String]
        | TargetsParam [String]
        | IterationParam Iteration
        | ScalersParam [Scalers]
        | MeasuresParam [Measure]
        | SubmovementsParam [[MovementParams]]
      deriving (Generic, Show)       

    instance ToJSON MovementParams
    instance FromJSON MovementParams

    fromMovementParams :: [MovementParams] -> Maybe Movement
    fromMovementParams ps = do
        description <- findDescriptionParam ps
        notes <- findNotesParam ps
        labels <- findLabelsParam ps
        targets <- findTargetsParam ps
        iteration <- findIterationParam ps
        scalers <- findScalersParam ps
        measures <- findMeasuresParam ps
        submovementsParams <- findSubmovementsParam ps        
        submovements <- mapM fromMovementParams submovementsParams
        return (Movement description notes labels targets iteration scalers measures submovements)
        where
            findDescriptionParam [] = Nothing
            findDescriptionParam (DescriptionParam d:_) = Just d
            findDescriptionParam (_:ps) = findDescriptionParam ps

            findNotesParam [] = Nothing
            findNotesParam (NotesParam s:_) = Just s
            findNotesParam (_:ps) = findNotesParam ps

            findLabelsParam [] = Nothing
            findLabelsParam (LabelsParam l:_) = Just l
            findLabelsParam (_:ps) = findLabelsParam ps

            findTargetsParam [] = Nothing
            findTargetsParam (TargetsParam t:_) = Just t
            findTargetsParam (_:ps) = findTargetsParam ps

            findIterationParam [] = Nothing
            findIterationParam (IterationParam i:_) = Just i
            findIterationParam (_:ps) = findIterationParam ps

            findScalersParam [] = Nothing
            findScalersParam (ScalersParam s:_) = Just s
            findScalersParam (_:ps) = findScalersParam ps

            findMeasuresParam [] = Nothing
            findMeasuresParam (MeasuresParam m:_) = Just m            
            findMeasuresParam (_:ps) = findMeasuresParam ps

            findSubmovementsParam [] = Nothing
            findSubmovementsParam (SubmovementsParam m:_) = Just m
            findSubmovementsParam (_:ps) = findSubmovementsParam ps

    data BlockIteration =
          Amrap Time
        | ForTime
        | ForTimeCap Time
        | Sets Integer
        | NoBlockIteration
        deriving (Generic, Show)

    instance ToJSON BlockIteration
    instance FromJSON BlockIteration

    data BlockMeasure =
          MeasureBlockReps Integer
        | MeasureBlockWeight Double
        | MeasureBlockDistance Integer
        | NoBlockMeasure
        deriving (Generic, Show)

    instance ToJSON BlockMeasure
    instance FromJSON BlockMeasure

    data Block = Block {
        blockId :: Integer,
        blockIteration :: BlockIteration,   
        blockMeasure :: BlockMeasure,
        blockNotes :: String,    
        subblocks :: [Subblock]
    } deriving (Generic, Show)

    instance ToJSON Block
    instance FromJSON Block

    data Subblock = Subblock {
        subblockId :: Integer,
        subblockIteration :: BlockIteration,   
        subblockMeasure :: BlockMeasure,
        subblockNotes :: String,    
        subblockMovements :: [Movement]
    } deriving (Generic, Show)

    instance ToJSON Subblock
    instance FromJSON Subblock

    data TrainingDay = TrainingDay {
        date :: Date,
        blocks :: [Block]
    } deriving (Generic, Show)

    instance ToJSON TrainingDay
    instance FromJSON TrainingDay