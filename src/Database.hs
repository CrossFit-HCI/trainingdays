{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Database where
    import Database.MongoDB ( connect, host, access, master, Document, Pipe, Action, Database, (=:), Collection, Value (..), findOne, insert, Select (select), look )
    import Data.Text (pack)
    import DataModel (TrainingDay (..), Block (..), BlockIteration (..), BlockMeasure (..), Subblock (..), Movement (..), Label (Tag), Target (..), Iteration (..), Scaler (..), Measure (..), Date (..), Time (..))
    import Control.Monad.IO.Class (MonadIO)
    import Control.Monad ((>=>))

    -------------------------
    -- Database Connection --  
    -------------------------
    databaseIP :: String
    databaseIP = "23.239.16.70"

    databaseName :: Database
    databaseName = pack "training_days"

    connectToDB :: IO Pipe
    connectToDB = connect (host databaseIP)

    runAction :: Show a => Action IO a -> IO ()
    runAction action = do
        pipe <- connectToDB
        e <- access pipe master databaseName action
        print e

    selectInsert :: (MonadIO m, MonadFail m) => Collection -> Document -> Action m Value
    -- ^ Inserts the given Document into Collection if it doesn't already exist. 
    selectInsert col doc = do
        query <- findOne $ select doc col
        case query of
            Nothing -> insert col doc
            Just d -> look (pack "_id") d

    selectInsertAll :: (MonadIO m, MonadFail m) => (a -> Action m Document) -> Collection -> [a] -> Action m [Value]
    -- ^ Like `selectInsert` but over a list of objects to be inserted.
    selectInsertAll toDoc col objs = mapM (toDoc >=> selectInsert col) objs

    ---------------------------
    -- Data Model Conversion --  
    ---------------------------

    timeToDoc :: Time -> Document
    timeToDoc (Time hours minutes seconds milliseconds) =
        [ "hours" =: hours,
          "minutes" =: minutes,
          "seconds" =: seconds,
          "milliseconds" =: milliseconds  ]

    dateToDoc :: Date -> Document
    dateToDoc (Date day month year) =
        [ "day" =: day,
          "month" =: month,
          "year" =: year ]

    labelToDoc :: Label -> Document
    labelToDoc label = ["description" =: labelToString label]
        where
            labelToString :: Label -> String
            labelToString (Tag t) = t

    targetToDoc :: Target -> Document
    targetToDoc target = ["description" =: targetToString target]
        where
            targetToString :: Target -> String
            targetToString (Target t) = t

    iterationToDoc :: Iteration -> Document
    iterationToDoc (IterateByReps r)= ["description" =: pack "reps", "value" =: r]
    iterationToDoc (IterateByDist d) = ["description" =: pack "distance", "value" =: d]
    iterationToDoc (IterateByCalories c) = ["description" =: pack "calories", "value" =: c]

    scalerToDoc :: Scaler -> Document
    scalerToDoc (ScaleDistance d) = ["description" =: pack "distance", "value" =: d]
    scalerToDoc (ScaleWeight w) = ["description" =: pack "weight", "value" =: w]
    scalerToDoc (ScaleIncreaseRoundReps r) = ["description" =: pack "increase_round_reps", "value" =: r]
    scalerToDoc (ScaleRPE (low,high)) = ["description" =: pack "rpe", "value" =: ["low" =: low, "high" =: high]]

    measureToDoc :: Measure -> Document
    measureToDoc (MeasureRepetitions r) = ["description" =: pack "reps", "value" =: r]
    measureToDoc (MeasureTime t) = ["description" =: pack "time", "value" =: timeToDoc t]
    measureToDoc (MeasureDistance d) = ["description" =: pack "distance", "value" =: d]
    measureToDoc (MeasureCalories c) = ["description" =: pack "calories", "value" =: c]
    measureToDoc (MeasureWeight w) = ["description" =: pack "weight", "value" =: w]

    movementToDoc :: Movement -> Action IO Document
    movementToDoc (Movement description notes labels targets _ _ _ submovements) = do
        submsIds <- selectInsertAll movementToDoc "movements" submovements
        labelsIds <- selectInsertAll (return . labelToDoc) "labels" labels
        targetsIds <- selectInsertAll (return . targetToDoc) "targets" targets

        return [ "description" =: description,
                "notes" =: notes,
                "labels" =: labelsIds,
                "targets" =: targetsIds,
                "submovements" =: submsIds
            ]

    blockMeasureToDoc :: BlockMeasure -> Document
    blockMeasureToDoc (MeasureBlockReps r) = ["description" =: pack "reps", "value" =: r]
    blockMeasureToDoc (MeasureBlockWeight w) = ["description" =: pack "weight", "value" =: w]
    blockMeasureToDoc (MeasureBlockDistance d) = ["description" =: pack "distance", "value" =: d]
    blockMeasureToDoc NoBlockMeasure = ["description" =: pack "none", "value" =: pack "none"]

    blockIterationToDoc :: BlockIteration -> Document
    blockIterationToDoc (Amrap t) = ["description" =: pack "amrap", "value" =: timeToDoc t]
    blockIterationToDoc ForTime = ["description" =: pack "for time", "value" =: pack "none"]
    blockIterationToDoc (ForTimeCap t) = ["description" =: pack "for time cap", "value" =: timeToDoc t]
    blockIterationToDoc (Sets s) = ["description" =: pack "sets", "value" =: s]
    blockIterationToDoc NoBlockIteration = ["description" =: pack "none", "value" =: pack "none"]

    subblockMovementToDoc :: Movement -> Action IO Document
    subblockMovementToDoc movement@(Movement _ _ _ _ iteration scalers measures _) = do
        id <- (movementToDoc >=> selectInsert "movement") movement
        return [ "movement" =: id,
                 "iteration" =: iterationToDoc iteration,
                 "scalers" =: map scalerToDoc scalers,
                 "measures" =: map measureToDoc measures
            ]

    subblockToDoc :: Subblock -> Action IO Document
    subblockToDoc (Subblock subblockId subblockIteration subblockMeasure subblockNotes subblockMovements ) = do
        movements <- mapM subblockMovementToDoc subblockMovements
        return [
            "id" =: subblockId,
            "iteration" =: blockIterationToDoc subblockIteration,
            "measure" =: blockMeasureToDoc subblockMeasure,
            "notes" =: pack subblockNotes,
            "movements" =: movements
         ]

    blockToDoc :: Block -> Action IO Document
    blockToDoc (Block blockId blockIteration blockMeasure blockNotes subblocks) = undefined

    trainingDayToDoc :: String -> TrainingDay -> Maybe Document
    trainingDayToDoc athleteId (TrainingDay date cycle []) = Nothing
    trainingDayToDoc athleteId (TrainingDay date cycle blocks) = undefined

    insertTrainingDay :: String -> TrainingDay -> Action IO ()
    insertTrainingDay athleteId trainingDay = undefined
