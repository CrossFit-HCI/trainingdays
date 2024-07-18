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
import Data.Functor
import Prelude hiding (cycle)

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

parse :: FilePath -> IO DM.TrainingDay
parse s = do
     f <- (readFile s)
     let cs = lexer f
     parser cs
     

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
                         | n == 0 = compareTokens t t'
     lookahead' _ _ = False

infixr <|>
(<|>) :: (Token, TokenParser a) -> TokenParser a -> TokenParser a
(<|>) (t1,parser1) parser2 = do
     isT1 <- lookahead t1
     if isT1
     then parser1
     else parser2

tokenParser :: Token -> String -> TokenParser TokenData
tokenParser t msg = do
     cs <- get
     case cs of
          (t':cs') ->
               if (compareTokens t t')
               then do
                    put cs'
                    return . getTokenData $ t'
               else parseError msg
          _ -> parseError msg

tokenUnitParser :: Token -> String -> TokenParser ()
tokenUnitParser t msg = do
     u <- tokenParser t msg
     case u of
          NoData -> return ()
          _ -> parseError msg

tokenStringParser :: (String -> Token) -> String -> TokenParser String
tokenStringParser t msg = do
     u <- tokenParser (t "") msg
     case u of
          StringData s -> return s
          _ -> parseError msg

tokenIntegerParser :: (Integer -> Token) -> String -> TokenParser Integer
tokenIntegerParser t msg = do
     u <- tokenParser (t 0) msg
     case u of
          IntegerData d -> return d
          _ -> parseError msg

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
          newline
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

colonEntryParser :: TokenParser () -> TokenParser a -> TokenParser a
colonEntryParser entryParser valueParser = do
     spaces
     entryParser
     colon
     spaces
     valueParser

trainingDayEntry :: TokenParser DM.TrainingDay
trainingDayEntry = colonEntryParser trainingDay $ do
     d <- date
     newline
     c <- cycleEntry
     newline
     bs <- blockList
     return $ DM.TrainingDay d c bs

cycleEntry :: TokenParser (Maybe DM.TrainingCycle)
cycleEntry = do
          dashOption
          colonEntryParser cycle $ do
               n <- lookahead TokenNone
               if n 
               then do
                    none 
                    return Nothing
               else do
                    newline
                    s <- colonEntryParser start date
                    newline
                    e <- colonEntryParser end date
                    return $ Just $ DM.TrainingCycle s e 1   

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
blockEntry = colonEntryParser block $ do
          blockID <- digits
          newline
          blockIteration <- blockIterationEntry
          blockMeasure <- blockMeasure
          blockNotes <- notesEntry
          subblocks <- subblockList' []
          return $ DM.Block blockID blockIteration blockMeasure blockNotes subblocks
  where
     subblockList' :: [DM.Subblock] -> TokenParser [DM.Subblock]
     subblockList' acc = do
          b <- lookahead2 TokenSubblock
          if b
          then do
               dashOption
               bl <- subblockEntry
               n <- lookahead TokenNewline
               if n
               then do
                    newline
                    subblockList' (bl:acc)
               else subblockList' (bl:acc)
          else return acc

subblockEntry :: TokenParser DM.Subblock
subblockEntry = colonEntryParser subblock $ do
          blockID <- digits
          newline
          blockIteration <- blockIterationEntry
          blockMeasure <- blockMeasure
          blockNotes <- notesEntry
          blockMovements <- movementsEntry
          return $ DM.Subblock blockID blockIteration blockMeasure blockNotes blockMovements

-- TODO: Need to add NoBlockIteration
blockIterationEntry :: TokenParser DM.BlockIteration
blockIterationEntry = dashEntry iteration iterationElement
  where
     iterationElement = (TokenSets,setsParser) <|>
                        (TokenAmrap,amrapParser) <|>
                        (TokenForTime,fortimeParser) <|>
                        fortimecapParser

     setsParser = colonEntryParser sets $ do
          d <- digits
          return $ DM.Sets d

     amrapParser = colonEntryParser amrap $ do
          t <- timeValue
          return $ DM.Amrap t

     fortimeParser = do
          fortime
          spaces
          return DM.ForTime

     fortimecapParser = colonEntryParser fortimecap $ do
          t <- timeValue
          return $ DM.ForTimeCap t

blockMeasure :: TokenParser DM.BlockMeasure
blockMeasure = colonEntryParser measure $ (TokenNewline,blockEntryParser) <|> noneParser
  where
     blockEntryParser = do
               newline
               dashOption
               m <- blockMeasureEntry
               newline
               return m

     blockMeasureEntry = (TokenReps,repsEntry DM.MeasureBlockReps) <|>
                         (TokenWeight,weightEntry DM.MeasureBlockWeight) <|>
                         (distanceEntry DM.MeasureBlockDistance)

     noneParser = do
               none
               newline
               return DM.NoBlockMeasure

distanceEntry :: (Double -> a) -> TokenParser a
distanceEntry c = colonEntryParser distance $ do
                    d <- decimal
                    return $ c d

movementsEntry :: TokenParser [DM.Movement]
movementsEntry = colonEntryParser movements $ do
     newline
     movementsEntryList []

-- Checking for a TokenDash on the lookahead doesn't catch the 
-- last movement which has a "- block" after.
movementsEntryList :: [DM.Movement] -> TokenParser [DM.Movement]
movementsEntryList acc = do
     m <- movementEntry
     b <- lookahead2 TokenMovement
     if b then do
          movementsEntryList (m:acc)
          else return (m:acc)

movementEntry :: TokenParser DM.Movement
movementEntry = do
     dashOption
     colonEntryParser movement $ do
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
movementParam = (TokenNotes,notesEntry <&> DM.NotesParam) <|>
                (TokenLabels,labelsEntry <&> DM.LabelsParam) <|>
                (TokenTargets,targetsEntry <&> DM.TargetsParam) <|>
                (TokenIteration,iterationEntry <&> DM.IterationParam) <|>
                (TokenScalers,scalersEntry <&> DM.ScalersParam) <|>
                (TokenMeasures,measuresEntry <&> DM.MeasuresParam) <|>
                (TokenSubmovements,submovementsEntry <&> DM.SubmovementsParam) <|>
                parseError "Incomplete movement specification."

submovementsEntry :: TokenParser [[DM.MovementParams]]
submovementsEntry = colonEntryParser submovements $ do
     none
     newline
     return []

measuresEntry :: TokenParser [DM.Measure]
measuresEntry = dashListEntry measures measureElement

measureElement :: TokenParser DM.Measure
measureElement = (TokenWeight,weightEntry DM.MeasureWeight)       <|>
                 (TokenReps,repsEntry DM.MeasureRepetitions)      <|>
                 (TokenTime,timeEntry DM.MeasureTime)             <|>
                 (TokenDistance,distanceEntry DM.MeasureDistance) <|>
                 (TokenCalories,calsEntry DM.MeasureCalories)     <|>
                 parseError "Incorrect measure."

calsEntry :: (Integer -> a) -> TokenParser a
calsEntry c = colonEntryParser calories $ do
     d <- digits
     return $ c d

weightEntry :: (Double -> a) -> TokenParser a
weightEntry c = colonEntryParser weight $ do
     d <- decimal
     return $ c d

timeEntry :: (DM.Time -> a) -> TokenParser a
timeEntry c = colonEntryParser time $ do
     t <- timeValue
     return $ c t

scalersEntry :: TokenParser [DM.Scaler]
scalersEntry = dashListEntry scalers scalerElement

scalerElement :: TokenParser (DM.Scaler)
scalerElement = (TokenDistance,distanceEntry DM.ScaleDistance) <|>
                (TokenWeight,weightEntry DM.ScaleWeight) <|>
                (TokenIncreaseRoundsByReps,increaseRoundsByRepsEntry) <|>
                (TokenRPE,rpeEntry DM.ScaleRPE) <|>                  
                parseError "Incorrect scaler."

increaseRoundsByRepsEntry :: TokenParser DM.Scaler
increaseRoundsByRepsEntry = colonEntryParser increaseRoundsByReps $ do
     d <- digits
     return $ DM.ScaleIncreaseRoundReps d

rpeEntry :: ((Integer,Integer) -> a) -> TokenParser a
rpeEntry c = colonEntryParser rpe $ do
     (low,high) <- range
     if 0 < low && low < 10 &&
        1 <= high && high <= 10
     then return $ c (low,high)
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
     iterationElement = (TokenReps,repsEntry DM.IterateByReps) <|> 
                        (TokenDistance,distanceEntry DM.IterateByDist) <|>
                        (TokenCalories,calsEntry DM.IterateByCalories) <|>
                        parseError "Expecting iteration entry."

repsEntry :: (Integer -> a) -> TokenParser a
repsEntry c = colonEntryParser reps $ do
               d <- digits
               return $ c d

constString :: (String -> a) -> TokenParser a
constString const = do
     s <- string
     return $ const s

labelsEntry :: TokenParser [DM.Label]
labelsEntry = dashListEntry labels $ constString DM.Tag

targetsEntry :: TokenParser [DM.Target]
targetsEntry = dashListEntry targets $ constString DM.Target

notesEntry :: TokenParser String
notesEntry = colonEntryParser notes $ do
     s <- string
     newline
     return s

spaces :: TokenParser ()
spaces = do
     cs <- get
     case cs of
          (TokenSpace:r) -> put r >> spaces
          _ -> return ()

timeValue :: TokenParser DM.Time
timeValue = do
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

decimal :: TokenParser Double
decimal = do
     d1 <- digits
     dot
     d2 <- digits
     return $ floatFromIntegers d1 d2

floatFromIntegers :: Integer -> Integer -> Double
floatFromIntegers d1 d2 = (fromInteger d1) + helper (fromInteger d2)
  where
     helper :: Double -> Double
     helper d | d > 1.0 = helper (d/10)
              | d <= 1.0 = d
              -- Never reached.
              | otherwise = 0.0

digits :: TokenParser Integer
digits = do
     d1 <- digit
     digitsAcc [d1]
 where
     digitsAcc :: [Integer] -> TokenParser Integer
     digitsAcc acc = do
          isDigit <- lookahead (TokenDigit 0)
          if isDigit
          then do d <- digit
                  digitsAcc (d:acc)
          else return $ digitsToInt (reverse acc)

digit :: TokenParser Integer
digit = tokenIntegerParser TokenDigit "Digit expected."

dot :: TokenParser ()
dot = tokenUnitParser TokenDot "Period expected."

backslash :: TokenParser ()
backslash = tokenUnitParser TokenBackslash "Backslash expected."

colon :: TokenParser ()
colon = tokenUnitParser TokenColon "Colon expected."

none :: TokenParser ()
none = tokenUnitParser TokenNone "none expected."

newline :: TokenParser ()
newline = spaces >> tokenUnitParser TokenNewline "newline expected."

space :: TokenParser ()
space = tokenUnitParser TokenSpace "space expected."

trainingDay :: TokenParser ()
trainingDay = tokenUnitParser TokenTrainingDay "TrainingDay expected."

subblock :: TokenParser ()
subblock = tokenUnitParser TokenSubblock "subblock expected."

block :: TokenParser ()
block = tokenUnitParser TokenBlock "block expected."

sets :: TokenParser ()
sets = tokenUnitParser TokenSets "sets expected."

iteration :: TokenParser ()
iteration = tokenUnitParser TokenIteration "iteration expected."

measure :: TokenParser ()
measure = tokenUnitParser TokenMeasure "measure expected."

notes :: TokenParser ()
notes = tokenUnitParser TokenNotes "notes expected."

string :: TokenParser String
string = tokenStringParser TokenString "string expected."

movements :: TokenParser ()
movements = tokenUnitParser TokenMovements "movements expected."

movement :: TokenParser ()
movement = tokenUnitParser TokenMovement "movement expected."

scalers :: TokenParser ()
scalers = tokenUnitParser TokenScalers "scalers expected."

labels :: TokenParser ()
labels = tokenUnitParser TokenLabels "labels expected."

targets :: TokenParser ()
targets = tokenUnitParser TokenTargets "targets expected."

reps :: TokenParser ()
reps = tokenUnitParser TokenReps "reps expected."

rpe :: TokenParser ()
rpe = tokenUnitParser TokenRPE "rpe expected."

measures :: TokenParser ()
measures = tokenUnitParser TokenMeasures "measures expected."

weight :: TokenParser ()
weight = tokenUnitParser TokenWeight "weight expected."

submovements :: TokenParser ()
submovements = tokenUnitParser TokenSubmovements "submovements expected."

dash :: TokenParser ()
dash = tokenUnitParser TokenDash "dash expected."

amrap :: TokenParser ()
amrap = tokenUnitParser TokenAmrap "amrap expected."

fortime :: TokenParser ()
fortime = tokenUnitParser TokenForTime "fortime expected."

fortimecap :: TokenParser ()
fortimecap = tokenUnitParser TokenForTimeCap "fortimecap expected."

distance :: TokenParser ()
distance = tokenUnitParser TokenDistance "distance expected."

time :: TokenParser ()
time = tokenUnitParser TokenTime "time expected."

calories :: TokenParser ()
calories = tokenUnitParser TokenCalories "calories expected."

cycle :: TokenParser ()
cycle = tokenUnitParser TokenCycle "cycle expected."

start :: TokenParser ()
start = tokenUnitParser TokenStart "start expected."

end :: TokenParser ()
end = tokenUnitParser TokenEnd "end expected."

increaseRoundsByReps :: TokenParser ()
increaseRoundsByReps = tokenUnitParser TokenIncreaseRoundsByReps "increase-rounds-by-reps expected."