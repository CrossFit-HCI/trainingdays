{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Database where
    import Database.MongoDB ( connect, host, access, master, Document, Pipe, Action, Database, (=:), Collection, Value (..), findOne, insert, Select (select), look, lookup, upsert, Val (val) )
    import Data.Text (pack)
    import DataModel (TrainingDay (..), Block (..), BlockIteration (..), Subblock (..), Movement (..), Label (Tag), Target (..), Iteration (..), Scaler (..), Measure (..), Date (..), Time (..), BlockMeasure)
    import Control.Monad.IO.Class (MonadIO)
    import Control.Monad ((>=>))
    import Prelude hiding (lookup)
    
    -------------------------
    -- Database Connection --  
    -------------------------
    databaseIP :: String
    databaseIP = "23.239.16.70"

    databaseName :: Database
    databaseName = pack "training_days_dev"

    connectToDB :: IO Pipe
    connectToDB = connect (host databaseIP)

    runAction :: Show a => Action IO a -> IO ()
    runAction action = do
        pipe <- connectToDB
        e <- access pipe master databaseName action
        print e

    -------------------------
    -- Database Queries    --
    -------------------------

    selectInsert :: (MonadIO m, MonadFail m) => Collection -> String -> Document -> Action m Value
    -- ^ Determines if `doc` is already in the database by selecting on `key`,
    -- and if it doesn't exist, then inserts it, but if it does exist, updates
    -- every field besides `key`. Throws an exception if `key` doesn't exist in `doc`.
    selectInsert col key doc = do
        let descMaybe = (lookup (pack key) doc :: Maybe String)
        case descMaybe of
            Nothing -> error $ "selectInsert: failed to find key "++key
            Just desc -> do 
                query <- findOne $ select [pack key =: desc] col 
                case query of
                    Nothing -> insert col doc
                    Just d -> upsert (select [pack key =: desc] col) doc >> look (pack "_id") d

    selectInsertAll :: (MonadIO m, MonadFail m) => (a -> Action m Document) -> Collection -> String -> [a] -> Action m [Value]
    -- ^ Like `selectInsert` but over a list of objects to be inserted.
    selectInsertAll toDoc col key objs = mapM (toDoc >=> selectInsert col key) objs

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
    measureToDoc (MeasureRepetitions _) = ["description" =: pack "reps"]
    measureToDoc (MeasureTime _) = ["description" =: pack "time"]
    measureToDoc (MeasureDistance _) = ["description" =: pack "distance"]
    measureToDoc (MeasureCalories _) = ["description" =: pack "calories"]
    measureToDoc (MeasureWeight _) = ["description" =: pack "weight"]
    -- measureToDoc (MeasureRepetitions r) = ["description" =: pack "reps", "value" =: r]
    -- measureToDoc (MeasureTime t) = ["description" =: pack "time", "value" =: timeToDoc t]
    -- measureToDoc (MeasureDistance d) = ["description" =: pack "distance", "value" =: d]
    -- measureToDoc (MeasureCalories c) = ["description" =: pack "calories", "value" =: c]
    -- measureToDoc (MeasureWeight w) = ["description" =: pack "weight", "value" =: w]

    movementToDoc :: Movement -> Action IO Document
    movementToDoc (Movement description notes labels targets _ _ _ submovements) = do
        submsIds <- selectInsertAll movementToDoc "movements" "description" submovements
        labelsIds <- selectInsertAll (return . labelToDoc) "labels" "description" labels
        targetsIds <- selectInsertAll (return . targetToDoc) "targets" "description" targets

        return [ "description" =: description,
                "notes" =: notes,
                "labels" =: labelsIds,
                "targets" =: targetsIds,
                "submovements" =: submsIds
            ]    

    blockIterationToDoc :: BlockIteration -> Document
    blockIterationToDoc (Amrap t) = ["description" =: pack "amrap", "value" =: timeToDoc t]
    blockIterationToDoc ForTime = ["description" =: pack "for time", "value" =: pack "none"]
    blockIterationToDoc (ForTimeCap t) = ["description" =: pack "for time cap", "value" =: timeToDoc t]
    blockIterationToDoc (Sets s) = ["description" =: pack "sets", "value" =: s]
    blockIterationToDoc NoBlockIteration = ["description" =: pack "none", "value" =: pack "none"]

    measureToValue :: Measure -> Value
    measureToValue (MeasureRepetitions r) = val r
    measureToValue (MeasureTime t) = val $ timeToDoc t
    measureToValue (MeasureDistance d) = val d
    measureToValue (MeasureCalories c) = val c
    measureToValue (MeasureWeight w) = val w                

    measureToIdValue :: Measure -> (Action IO) Document
    measureToIdValue m = do
        let v = measureToValue m
        id <- selectInsert "measure" "description" (measureToDoc m)
        return ["id" =: id, "value" =: v]

    measureToIdValueMany :: [Measure] -> Action IO [Document]
    measureToIdValueMany ms = mapM measureToIdValue ms      

    subblockMovementToDoc :: Movement -> Action IO Document
    subblockMovementToDoc movement@(Movement _ _ _ _ iteration scalers measures _) = do
        id <- (movementToDoc >=> selectInsert "movement" "description") movement
        measuresDoc <- measureToIdValueMany measures
        return [ "movement" =: id,
                 "iteration" =: iterationToDoc iteration,
                 "scalers" =: map scalerToDoc scalers,
                 "measures" =: measuresDoc
            ]

    subblockMeasureToIdValue :: BlockMeasure -> Action IO Document
    subblockMeasureToIdValue Nothing = return []
    subblockMeasureToIdValue (Just m) = measureToIdValue m

    subblockToDoc :: Subblock -> Action IO Document
    subblockToDoc (Subblock subblockId subblockIteration subblockMeasure subblockNotes subblockMovements ) = do
        movements <- mapM subblockMovementToDoc subblockMovements
        measureDoc <- subblockMeasureToIdValue subblockMeasure
        return [
            "id" =: subblockId,
            "iteration" =: blockIterationToDoc subblockIteration,
            "measure" =: measureDoc, 
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
