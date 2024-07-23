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
        | MeasureDistance Double
        | MeasureCalories Integer
        | MeasureWeight Double
        deriving (Generic, Show)

    instance ToJSON Measure
    instance FromJSON Measure

    data MovementIteration = 
        IterateByReps Integer
      | IterateByDist Double
      | IterateByCalories Integer
      deriving (Generic, Show)

    instance ToJSON MovementIteration
    instance FromJSON MovementIteration

    data Scaler = 
        ScaleDistance Double
      | ScaleWeight Double
      | ScaleIncreaseRoundReps Integer
      | ScaleRPE (Integer,Integer)
      deriving (Generic, Show)

    instance ToJSON Scaler
    instance FromJSON Scaler

    newtype Label = Tag String
      deriving (Generic, Show)

    instance ToJSON Label
    instance FromJSON Label

    newtype Target = Target String
      deriving (Generic, Show)    

    instance ToJSON Target
    instance FromJSON Target

    -- TODO: Need to add equipment
    data Movement = Movement {
        description :: String,
        notes :: String,
        labels :: [Label],
        targets :: [Target],
        iteration :: MovementIteration,
        scalers :: [Scaler],
        measures :: [Measure],
        submovements :: [Movement]
    } deriving (Generic, Show)

    instance ToJSON Movement
    instance FromJSON Movement

    data MovementParams =
          DescriptionParam String
        | NotesParam String
        | LabelsParam [Label]
        | TargetsParam [Target]
        | IterationParam MovementIteration
        | ScalersParam [Scaler]
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

    type BlockMeasure = Maybe Measure
  
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
        cycle :: Maybe TrainingCycle,
        blocks :: [Block]
    } deriving (Generic, Show)

    instance ToJSON TrainingDay
    instance FromJSON TrainingDay

    data TrainingCycle = TrainingCycle {
      -- The start of the cycle.
      cycleStartDate :: Date,
      -- The end of the cycle.
      cycleEndDate :: Date,
      -- Length in days.
      cycleLength :: Integer
    } deriving (Generic, Show)

    instance ToJSON TrainingCycle
    instance FromJSON TrainingCycle

    data TrainingJournal = TrainingJournal {
      journalTitle :: String,
      journalDescription :: String,
      journalTraining :: [TrainingDay]
    } deriving (Generic, Show)

    instance ToJSON TrainingJournal
    instance FromJSON TrainingJournal

    data Athlete = Athlete {
      firstName :: String,
      lastName :: String,
      email :: String,
      journals :: [TrainingJournal]
    } deriving (Generic, Show)

    instance ToJSON Athlete
    instance FromJSON Athlete

