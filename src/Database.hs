{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Database where
    import Database.MongoDB ( connect, host, access, master, Document, Pipe, Action, Database, (=:) )
    import Data.Text (pack)
    import DataModel (TrainingDay (..), Block (..), BlockIteration, BlockMeasure, Subblock (..), Movement (..), Label (Tag), Target (..), Iteration (..), Scaler (..), Measure (..), Date (..), Time (..))
    
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

    labelsToDoc :: [Label] -> [String]
    labelsToDoc labels = map labelToString labels
        where
            labelToString :: Label -> String
            labelToString (Tag t) = t

    targetsToDoc :: [Target] -> [String]
    targetsToDoc targets = map targetToString targets
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
    measureToDoc (MeasureWeight w) = ["description" =: pack "measure", "value" =: w]

    movementToDoc :: Movement -> Document
    movementToDoc (Movement description notes labels targets iteration scalers measures submovements) = 
        [ "description" =: description,
          "notes" =: notes,
          "labels" =: labelsToDoc labels,
          "targets" =: targetsToDoc targets,
          "iteration" =: iterationToDoc iteration,
          "scalers" =: map scalerToDoc scalers,
          "measures" =: map measureToDoc measures,
          "submovements" =: map movementToDoc submovements
        ]

    blockMeasureToDoc :: BlockMeasure -> Document
    blockMeasureToDoc = undefined

    blockIterationToDoc :: BlockIteration -> Document
    blockIterationToDoc = undefined

    subblockToDoc :: Subblock -> Document
    subblockToDoc (Subblock subblockId subblockIteration subblockMeasure subblockNotes subblockMovements ) = undefined

    blockToDoc :: Block -> Document
    blockToDoc (Block blockId blockIteration blockMeasure blockNotes subblocks) = undefined    

    trainingDayToDoc :: String -> TrainingDay -> Maybe Document
    trainingDayToDoc athleteId (TrainingDay date []) = Nothing
    trainingDayToDoc athleteId (TrainingDay date blocks) = undefined

    insertTrainingDay :: String -> TrainingDay -> Action IO ()
    insertTrainingDay athleteId trainingDay = undefined
