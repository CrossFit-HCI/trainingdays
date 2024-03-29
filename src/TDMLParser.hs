{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Use <$>" #-}
module TDMLParser where
import qualified DataModel as DM
import TDMLLexer

import Control.Monad.State.Lazy

type TokenParser a = StateT [Token] IO a

parser :: [Token] -> IO DM.TrainingDay
parser = evalStateT trainingDayEntry

testParser :: TokenParser a -> String -> IO (a,[Token])
testParser p s = runStateT p $ lexer s

testParserFile :: FilePath -> IO ()
testParserFile s = do
     f <- (readFile s)
     let cs = lexer f
     let t = parser cs
     t >>= print

parseError :: String -> TokenParser a
parseError msg = do 
     cs <- get
     error $ "Parse error: " ++ msg ++ "\nRemaining Tokens: " ++ (show cs)

lookahead :: Token -> TokenParser Bool
lookahead = lookaheadN 0

lookahead2 :: Token -> TokenParser Bool
lookahead2 = lookaheadN 1

lookaheadN :: Int -> Token -> TokenParser Bool
lookaheadN n t = do
     cs <- get
     return $ lookahead' n cs
 where
     lookahead' :: Int -> [Token] -> Bool     
     lookahead' n (TokenSpace:r) = lookahead' n r
     lookahead' n (t':r) | n > 0 = lookahead' (n-1) r
                         | n == 0 = t == t'
     lookahead' _ _ = False

dashOption :: TokenParser () 
dashOption = do
     spaces
     dash
     spaces

dashEntry :: TokenParser () -> TokenParser a -> TokenParser a
dashEntry nameToken elementParser = do
     spaces
     nameToken
     colon
     spaces
     newline
     dashOption
     e <- elementParser
     newline
     return e

dashListEntry :: TokenParser () -> TokenParser a -> TokenParser [a]
dashListEntry listNameToken elementParser = do
     spaces
     listNameToken
     colon
     spaces
     b <- lookahead TokenNone
     if b then do
          none
          return []
     else do
          newline
          dashListParser elementParser

dashListParser :: TokenParser a -> TokenParser [a]
dashListParser elementParser = do
     dashOption
     e <- elementParser
     n <- lookahead TokenNewline
     if n
     then do
          newline     
          b <- lookahead TokenDash
          if b 
          then do
               r <- dashListParser elementParser
               return (e:r)
          else return [e]
     else return [e]

trainingDayEntry :: TokenParser DM.TrainingDay
trainingDayEntry = do
     trainingDay
     colon
     spaces
     d <- date
     newline
     bs <- blockList
     return $ DM.TrainingDay d bs

blockList :: TokenParser [DM.Block]
blockList = blockList' []
  where
     blockList' :: [DM.Block] -> TokenParser [DM.Block]
     blockList' acc = do
          b <- lookahead2 TokenBlock  
          if b 
          then do
               dashOption
               bl <- blockEntry
               n <- lookahead TokenNewline
               if n
               then do
                    newline
                    blockList' (bl:acc)
               else blockList' (bl:acc)
          else return acc

blockEntry :: TokenParser DM.Block
blockEntry = do
     block
     colon
     spaces
     blockID <- digits
     newline
     blockIteration <- blockIterationEntry
     blockMeasure <- blockMeasureEntry
     blockNotes <- notesEntry
     blockMovements <- movementsEntry
     return $ DM.Block blockID blockIteration blockMeasure blockNotes blockMovements

blockIterationEntry :: TokenParser DM.BlockIteration
blockIterationEntry = do
     dashEntry iteration iterationElement
  where
     iterationElement = do
          isSets <- lookahead TokenSets
          if isSets
          then do
               sets
               colon
               spaces
               d <- digits
               return $ DM.Sets d
          else do
               isAmrap <- lookahead TokenAmrap
               if isAmrap
               then do
                    amrap
                    colon
                    spaces
                    t <- time
                    return $ DM.Amrap t
               else do 
                    isForTime <- lookahead TokenForTime
                    if isForTime
                    then do
                         fortime
                         spaces
                         return $ DM.ForTime
                    else do
                         fortimecap
                         colon
                         spaces
                         t <- time
                         return $ DM.ForTimeCap t


blockMeasureEntry :: TokenParser DM.BlockMeasure
blockMeasureEntry = do
     spaces
     measure
     colon
     spaces
     none
     newline
     return DM.NoBlockMeasure

movementsEntry :: TokenParser [DM.Movement]
movementsEntry = do
     spaces
     movements
     colon
     spaces
     newline
     movementsEntryList []

-- Checking for a TokenDash on the lookahead doesn't catch the 
-- last movement which has a "- block" after.
movementsEntryList :: [DM.Movement] -> TokenParser [DM.Movement]
movementsEntryList acc = do
     m <- movementEntry
     b <- lookahead2 TokenMovement
     if b then movementsEntryList (m:acc)
          else return (m:acc)

movementEntry :: TokenParser DM.Movement
movementEntry = do
     dashOption
     movement
     colon
     spaces
     description <- string
     newline
     ps <- movementParamList [DM.DescriptionParam description]
     let maybeMovement = DM.fromMovementParams ps
     case maybeMovement of
          Just m -> return m
          Nothing -> parseError "Incomplete movement"

movementParamList :: [DM.MovementParams] -> TokenParser [DM.MovementParams]
movementParamList acc = do
     spaces
     cs <- get
     case cs of
          (t:_) -> if t `elem` paramTokens
                   then do p <- movementParam
                           movementParamList (p:acc)
                   else return acc
          _ -> return acc

  where
     paramTokens = [TokenNotes, TokenLabels,TokenTargets,
                    TokenIteration, TokenScalers, TokenMeasures, 
                    TokenSubmovements]

movementParam :: TokenParser DM.MovementParams
movementParam = do
     cs <- get
     case cs of
          (TokenNotes:_) -> do 
               n <- notesEntry
               return $ DM.NotesParam n
          (TokenLabels:_) -> do
               l <- labelsEntry
               return $ DM.LabelsParam l
          (TokenTargets:_) -> do
               t <- targetsEntry
               return $ DM.TargetsParam t
          (TokenIteration:_) -> do
               i <- iterationEntry
               return $ DM.IterationParam i
          (TokenScalers:_) -> do
               s <- scalersEntry
               return $ DM.ScalersParam s
          (TokenMeasures:_) -> do
               m <- measuresEntry
               return $ DM.MeasuresParam m
          (TokenSubmovements:_) -> do
               v <- submovementsEntry
               return $ DM.SubmovementsParam v
          _ -> parseError "Incomplete movement specification."

submovementsEntry :: TokenParser [[DM.MovementParams]]
submovementsEntry = do
     spaces
     submovements
     colon
     spaces
     none
     newline
     return []

measuresEntry :: TokenParser [DM.Measure]
measuresEntry = dashListEntry measures measureElement

measureElement :: TokenParser DM.Measure
measureElement = do
     weight
     colon
     spaces
     d <- digits
     return $ DM.MeasureWeight d

scalersEntry :: TokenParser [DM.Scalers]
scalersEntry = dashListEntry scalers scalerElement

scalerElement :: TokenParser (DM.Scalers)
scalerElement = do
     rpe
     colon
     spaces
     (low,high) <- range
     if 0 < low && low < 10 &&
        1 <= high && high <= 10
     then return $ DM.ScaleRPE (low,high)
     else parseError "Incorrect RPE range."

range :: TokenParser (Integer,Integer)
range = do
     low <- digits
     dash
     high <- digits
     if 0 <= low && low < high
     then return (low,high)
     else parseError "Range expected" 

iterationEntry :: TokenParser DM.Iteration
iterationEntry = dashEntry iteration iterationElement
 where
     iterationElement = do
          b <- lookahead TokenReps
          if b then do
               reps
               colon
               spaces
               d <- digits
               return $ DM.IterateByReps d
          else parseError "Expecting iteration entry."

labelsEntry :: TokenParser [String]
labelsEntry = dashListEntry labels string

targetsEntry :: TokenParser [String]
targetsEntry = do
     ts <- dashListEntry targets string
     newline
     return ts

notesEntry :: TokenParser String
notesEntry = do
     spaces
     notes
     colon
     spaces
     s <- string
     newline
     return s

spaces :: TokenParser ()
spaces = do
     cs <- get
     case cs of
          (TokenSpace:r) -> put r >> spaces
          _ -> return ()

time :: TokenParser DM.Time
time = do
     h1 <- digit
     h2 <- digit
     colon
     m1 <- digit
     m2 <- digit
     colon
     s1 <- digit
     s2 <- digit
     let h = digitsToInt [h1,h2]
     let m = digitsToInt [m1,m2]
     let s = digitsToInt [s1,s2]
     return $ DM.Time h m s 0

date :: TokenParser DM.Date
date = do
     m <- month
     backslash
     d <- day
     backslash
     y <- year
     return $ DM.Date d m y

month :: TokenParser Integer
month = do
     n1 <- digit
     n2 <- digit
     if n1 >= 0 && n1 <= 9 && 
        n2 >= 1 && n2 <= 9
     then return $ digitsToInt [n1,n2]
     else parseError "Expecting a month." 

day :: TokenParser Integer
day = do
     n1 <- digit
     n2 <- digit
     if n1 >= 0 && n1 <= 9 && 
        n2 >= 1 && n2 <= 9
     then return $ digitsToInt [n1,n2]
     else parseError "Expecting a day." 

year :: TokenParser Integer
year = do
     n1 <- digit
     n2 <- digit
     n3 <- digit
     n4 <- digit
     if n1 >= 1 && n1 <= 9 && 
        n2 >= 0 && n2 <= 9 &&
        n3 >= 0 && n3 <= 9 &&
        n4 >= 0 && n2 <= 9
     then return $ digitsToInt [n1,n2,n3,n4]
     else parseError "Expecting a year." 

digits :: TokenParser Integer
digits = do
     cs <- get
     case cs of
          (TokenDigit _:_) -> digitsAcc []
          _ -> parseError "Expecting digits."
 where
     digitsAcc :: [Integer] -> TokenParser Integer
     digitsAcc acc = do
          d <- digit
          cs <- get
          case cs of
               -- Lookahead to see if we need to keep parsing digits:
               (TokenDigit _:_) -> digitsAcc (d:acc)
               _ -> return $ digitsToInt (reverse (d:acc))

digit :: TokenParser Integer
digit = do
     cs <- get
     case cs of
          ((TokenDigit d):cs') -> do
               put cs'
               return d
          _ -> parseError "Digit expected."

backslash :: TokenParser ()
backslash = do
     cs <- get
     case cs of
          (TokenBackslash:cs') -> do
               put cs'
               return ()
          _ -> parseError "Backslash expected."

colon :: TokenParser ()
colon = do
     cs <- get
     case cs of
          (TokenColon:cs') -> do
               put cs'
               return ()
          _ -> parseError "Colon expected."

none :: TokenParser ()
none = do
     cs <- get
     case cs of
          (TokenNone:cs') -> do
               put cs'
               return ()
          _ -> parseError "none expected."

newline :: TokenParser ()
newline = do
     spaces
     cs <- get
     case cs of
          (TokenNewline:cs') -> do
               put cs'
               return ()
          _ -> parseError "newline expected."

space :: TokenParser ()
space = do
     cs <- get
     case cs of
          (TokenSpace:cs') -> do
               put cs'
               return ()
          _ -> parseError "space expected."

trainingDay :: TokenParser ()
trainingDay = do
     cs <- get
     case cs of
          (TokenTrainingDay:cs') -> do
               put cs'
               return ()
          _ -> parseError "TrainingDay expected."

block :: TokenParser ()
block = do
     cs <- get
     case cs of
          (TokenBlock:cs') -> do
               put cs'
               return ()
          _ -> parseError "block expected."

sets :: TokenParser ()
sets = do
     cs <- get
     case cs of
          (TokenSets:cs') -> do
               put cs'
               return ()
          _ -> parseError "sets expected."

iteration :: TokenParser ()
iteration = do
     cs <- get
     case cs of
          (TokenIteration:cs') -> do
               put cs'
               return ()
          _ -> parseError "iteration expected."

measure :: TokenParser ()
measure = do
     cs <- get
     case cs of
          (TokenMeasure:cs') -> do
               put cs'
               return ()
          _ -> parseError "measure expected."

notes :: TokenParser ()
notes = do
     cs <- get
     case cs of
          (TokenNotes:cs') -> do
               put cs'
               return ()
          _ -> parseError "notes expected."

string :: TokenParser String
string = do
     cs <- get
     case cs of
          (TokenString s:cs') -> do
               put cs'
               return s
          _ -> parseError "string expected."

movements :: TokenParser ()
movements = do
     cs <- get
     case cs of
          (TokenMovements:cs') -> do
               put cs'
               return ()
          _ -> parseError "movements expected."

movement :: TokenParser ()
movement = do
     cs <- get
     case cs of
          (TokenMovement:cs') -> do
               put cs'
               return ()
          _ -> parseError "movement expected."

scalers :: TokenParser ()
scalers = do
     cs <- get
     case cs of
          (TokenScalers:cs') -> do
               put cs'
               return ()
          _ -> parseError "scalers expected."


labels :: TokenParser ()
labels = do
     cs <- get
     case cs of
          (TokenLabels:cs') -> do
               put cs'
               return ()
          _ -> parseError "labels expected."

targets :: TokenParser ()
targets = do
     cs <- get
     case cs of
          (TokenTargets:cs') -> do
               put cs'
               return ()
          _ -> parseError "notes expected."

reps :: TokenParser ()
reps = do
     cs <- get
     case cs of
          (TokenReps:cs') -> do
               put cs'
               return ()
          _ -> parseError "reps expected."

rpe :: TokenParser ()
rpe = do
     cs <- get
     case cs of
          (TokenRPE:cs') -> do
               put cs'
               return ()
          _ -> parseError "rpe expected."

measures :: TokenParser ()
measures = do
     cs <- get
     case cs of
          (TokenMeasures:cs') -> do
               put cs'
               return ()
          _ -> parseError "measures expected."

weight :: TokenParser ()
weight = do
     cs <- get
     case cs of
          (TokenWeight:cs') -> do
               put cs'
               return ()
          _ -> parseError "weight expected."

submovements :: TokenParser ()
submovements = do
     cs <- get
     case cs of
          (TokenSubmovements:cs') -> do
               put cs'
               return ()
          _ -> parseError "submovements expected."

dash :: TokenParser ()
dash = do
     cs <- get
     case cs of
          (TokenDash:cs') -> do
               put cs'
               return ()
          _ -> parseError "dash expected."

amrap :: TokenParser ()
amrap = do
     cs <- get
     case cs of
          (TokenAmrap:cs') -> do
               put cs'
               return ()
          _ -> parseError "amrap expected."

fortime :: TokenParser () 
fortime = do
     cs <- get
     case cs of
          (TokenForTime:cs') -> do
               put cs'
               return ()
          _ -> parseError "fortime expected."

fortimecap :: TokenParser () 
fortimecap = do
     cs <- get
     case cs of
          (TokenForTimeCap:cs') -> do
               put cs'
               return ()
          _ -> parseError "fortimecap expected."
