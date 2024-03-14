{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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