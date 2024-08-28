{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Redundant $" #-}

module Database where
    import Database.MongoDB
        ( access,
        master,
        Document,
        Pipe,
        Action,
        Database,
        (=:),
        Collection,
        Value(..),
        findOne,
        insert,
        Select(select),
        look,
        lookup,
        upsert,
        Val(val),
        primary,
        secondaryOk )
    import Data.Text (pack, unpack)
    import DataModel (TrainingDay (..), Block (..), BlockIteration (..), Subblock (..), Movement (..), Label (Tag), Target (..), MovementIteration (..), Scaler (..), Measure (..), Date (..), Time (..), BlockMeasure, TrainingCycle (..), TrainingJournal (..), Athlete (..))
    import Control.Monad.IO.Class (MonadIO (liftIO))
    import Control.Monad ((>=>))
    import Prelude hiding (lookup)
    import Control.Exception (SomeException(SomeException))
    import qualified Data.Text as T
    import Database.MongoDB.Connection (openReplicaSetSRV')
    import Control.Exception.Base (try)
    import ResultST (ResultST, Error (ParseError, DBError), throwError, Result)
    import Utils (maybeCase)
    import Database.MongoDB.Query (auth)
    
    type StoreDB = Pipe
    type ResultDB a = ResultST StoreDB a

    databaseName :: Database
    databaseName = pack "training_days_dev"

    -------------------------
    -- Database Connection --  
    -------------------------

    data MongoAtlas = MongoAtlas {
        atlas_host :: T.Text,
        atlas_user :: T.Text,
        atlas_password :: T.Text
    } deriving Show

    extractMongoAtlasCredentials :: T.Text -> Result MongoAtlas
    extractMongoAtlasCredentials cs = do
        let s   = T.drop 14 cs
        let us' = T.splitOn ":" s
        case us' of
            [u,s'] -> case T.splitOn "@" s' of
                        [p,h] -> return $ MongoAtlas h u p
                        _ -> throwError . ParseError $ parseErrorMsg
            _ -> throwError . ParseError $ parseErrorMsg
        where
            parseErrorMsg = "extractMongoAtlasCredentials: Failed to parse credentials from input"    

    connectAtlas :: T.Text -> Result Pipe
    connectAtlas host = do        
        repset <- liftIO . openReplicaSetSRV' . unpack $ host
        ps <- liftIO $ primaryOrSecondary repset
        maybeCase ps 
            (throwError . DBError $ "Unable to acquire pipe from MongoDB Atlas' replicaset")
            return
            
        where
            primaryOrSecondary rep =
                try (primary rep) >>= \case
                Left (SomeException err) -> do
                    try (secondaryOk rep) >>= \case
                        Left (SomeException _) -> pure Nothing
                        Right pipe -> pure $ Just pipe
                Right pipe -> pure $ Just pipe

    authAtlas :: T.Text -> T.Text -> Pipe -> IO Bool
    authAtlas username password pipe = 
        access pipe master "admin" $
            auth username password

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

    iterationToDoc :: MovementIteration -> Document
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

    measureToIdValue :: Measure -> Action IO Document
    measureToIdValue m = do
        let v = measureToValue m
        id <- selectInsert "measure" "description" (measureToDoc m)
        return ["id" =: id, "value" =: v]

    measureToIdValueMany :: [Measure] -> Action IO [Document]
    measureToIdValueMany = mapM measureToIdValue

    iterationToValue :: MovementIteration -> Value
    iterationToValue (IterateByReps r)  = val r
    iterationToValue (IterateByDist d) = val d
    iterationToValue (IterateByCalories c) = val c

    iterationToIdValue :: MovementIteration -> Action IO Document
    iterationToIdValue i = do
        let v = iterationToValue i
        id <- selectInsert "movement-iteration" "description" (iterationToDoc i)
        return ["id" =: id, "value" =: v]

    iterationToIdValueMany :: [MovementIteration] -> Action IO [Document]
    iterationToIdValueMany  = mapM iterationToIdValue

    scalerToValue :: Scaler -> Value
    scalerToValue (ScaleDistance d) = val d
    scalerToValue (ScaleWeight w) = val w
    scalerToValue (ScaleIncreaseRoundReps r) = val r
    scalerToValue (ScaleRPE (low,high)) = val ["low" =: low, "high" =: high]

    scalerToIdValue :: Scaler -> Action IO Document
    scalerToIdValue s = do
        let v = scalerToValue s
        id <- selectInsert "scaler" "description" (scalerToDoc s)
        return ["id" =: id, "value" =: v]

    scalerToIdValueMany :: [Scaler] -> Action IO [Document]
    scalerToIdValueMany = mapM scalerToIdValue

    subblockMovementToDoc :: Movement -> Action IO Document
    subblockMovementToDoc movement@(Movement _ _ _ _ iteration scalers measures _) = do
        id <- (movementToDoc >=> selectInsert "movement" "description") movement
        measuresDoc <- measureToIdValueMany measures
        iterationDoc <- iterationToIdValue iteration
        scalersDoc <- scalerToIdValueMany scalers

        return [ "movement" =: id,
                 "iteration" =: iterationDoc,
                 "scalers" =: scalersDoc,
                 "measures" =: measuresDoc
            ]

    blockMeasureToIdValue :: BlockMeasure -> Action IO Document
    blockMeasureToIdValue Nothing = return []
    blockMeasureToIdValue (Just m) = measureToIdValue m

    blockIterationToValue :: BlockIteration -> Value
    blockIterationToValue (Amrap t) = val $ timeToDoc t
    blockIterationToValue ForTime = val $ pack "for time"
    blockIterationToValue (ForTimeCap t) = val $ timeToDoc t
    blockIterationToValue (Sets s) = val s
    blockIterationToValue NoBlockIteration = val $ pack "none"

    blockIterationToIdValue :: BlockIteration -> Action IO Document
    blockIterationToIdValue b = do
        let v = blockIterationToValue b
        id <- selectInsert "block-iteration" "description" (blockIterationToDoc b)
        return ["id" =: id, "value" =: v]

    subblockToDoc :: Subblock -> Action IO Document
    subblockToDoc (Subblock subblockId subblockIteration subblockMeasure subblockNotes subblockMovements) = do
        movements <- mapM subblockMovementToDoc subblockMovements
        measureDoc <- blockMeasureToIdValue subblockMeasure
        iterationDoc <- blockIterationToIdValue subblockIteration
        return [
            "id" =: subblockId,
            "iteration" =: iterationDoc,
            "measure" =: measureDoc,
            "notes" =: pack subblockNotes,
            "movements" =: movements
         ]

    blockToDoc :: Block -> Action IO Document
    blockToDoc (Block blockId blockIteration blockMeasure blockNotes subblocks) = do
        iterationDoc <- blockIterationToIdValue blockIteration
        measureDoc <- blockMeasureToIdValue blockMeasure
        subblocksDoc <- mapM subblockToDoc subblocks
        return ["id" =: blockId,
                "iteration" =: iterationDoc,
                "measure" =: measureDoc,
                "notes" =: pack blockNotes,
                "subblocks" =: subblocksDoc]

    cycleToDoc :: Maybe TrainingCycle -> Document
    cycleToDoc Nothing = ["description" =: pack "none", "value" =: pack "none"]
    cycleToDoc (Just (TrainingCycle start end length)) =
        ["start-date" =: dateToDoc start,
         "end-date" =: dateToDoc end,
         "length" =: length]

    trainingDayToDoc :: Value -> TrainingDay -> Action IO Document
    trainingDayToDoc athleteId (TrainingDay date cycle blocks) = do
        blocksDoc <- mapM blockToDoc blocks
        return ["athlete_id" =: athleteId,
                "date" =: dateToDoc date,
                "cycle" =: cycleToDoc cycle,
                "blocks" =: blocksDoc]

    trainingDayToId :: Value -> TrainingDay -> Action IO Value
    trainingDayToId athleteId trainingDay =
        trainingDayToDoc athleteId trainingDay >>= selectInsert "training-days" "athlete_id"

    trainingJournalToDoc :: Value -> TrainingJournal -> Action IO Document
    trainingJournalToDoc athleteId (TrainingJournal title description training) = do
        trainingIds <- mapM (trainingDayToId athleteId) training
        return [ "athlete_id" =: athleteId,
                 "title" =: pack title,
                 "description" =: pack description,
                 "training" =: trainingIds
               ]

    insertAthlete :: Athlete -> Action IO ()
    insertAthlete (Athlete first last email journals) = do
        -- 1. Select on email to determine if the athlete is already in the DB.
        query <- findOne $ select ["email" =: email] "athletes"
        case query of
            -- 2. If they are, then update the journals using the existing athlete_id.
            Just d -> do
                let athleteIdM = look (pack "_id") d
                case athleteIdM of
                    Just athleteId -> do
                        journalsDoc <- mapM (trainingJournalToDoc athleteId) journals
                        let athleteDoc = ["first-name" =: first, "last-name" =: last, "email" =: email, "journals" =: journalsDoc]
                        upsert (select ["_id" =: athleteId] "athletes") athleteDoc
                    Nothing -> error "insertAthlete: Failed to find athlete_id."
            -- 3. If they are not, then add them with an empty journals entry and return the id.
            Nothing -> do
                athleteId <- insert "athletes" ["first-name" =: first, "last-name" =: last, "email" =: email]
                -- 4. Using the id, build the journal's doc and insert it using the id from 3.               
                journalsDoc <- mapM (trainingJournalToDoc athleteId) journals
                let athleteDoc = ["first-name" =: first, "last-name" =: last, "email" =: email, "journals" =: journalsDoc]
                upsert (select ["_id" =: athleteId] "athletes") athleteDoc