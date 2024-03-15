{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module TDMLParser where
import qualified DataModel as DM
import Data.Char
import Control.Monad.State.Lazy

type TokenParser a = State [Token] a

parser :: [Token] -> DM.TrainingDay
parser = undefined

testParser :: String -> TokenParser a -> (a,[Token])
testParser s p = runState p $ lexer s

lookahead :: Token -> TokenParser Bool
lookahead t = do
     cs <- get
     return (case cs of
               (t':_) -> t == t'
               _ -> False)

range :: TokenParser (Integer,Integer)
range = do
     low <- digits
     dash
     high <- digits
     if 0 < low && low < high
     then return (low,high)
     else get >>= parseError "Range expected" 

iterationEntry :: TokenParser DM.Iteration
iterationEntry = do
     spaces
     iteration
     colon
     spaces
     newline
     dashOption
     cs <- get
     case cs of
          (TokenReps:_) -> do
               reps
               colon
               spaces
               d <- digits
               return $ DM.IterateByReps d
          _ -> parseError "Expecting iteration entry." cs

labelsEntry :: TokenParser [String]
labelsEntry = do
     spaces
     labels
     colon
     spaces
     b <- lookahead TokenNone
     if b then do
          none
          return []
     else do
          newline
          dashOptionStringList

targetsEntry :: TokenParser [String]
targetsEntry = do
     spaces
     targets
     colon
     spaces
     b <- lookahead TokenNone
     if b then do
          none
          return []
     else do
          newline
          dashOptionStringList

dashOptionStringList :: TokenParser [String]
dashOptionStringList = do
     spaces
     t <- dashOptionString
     newline
     spaces
     b <- lookahead TokenDash
     if b then do
          ts <- dashOptionStringList
          return (t:ts)
     else return [t]

dashOptionString :: TokenParser String
dashOptionString = do
     dashOption
     string

notesEntry :: TokenParser String
notesEntry = do
     notes
     colon
     spaces
     string

dashOption :: TokenParser () 
dashOption = do
     b <- lookahead TokenSpace
     if b then do
          spaces
          dash
          spaces
     else do
          dash
          spaces

spaces :: TokenParser ()
spaces = do
     b1 <- lookahead TokenSpace
     if b1 then space
     else return ()
     b2 <- lookahead TokenSpace
     if b2 then spaces
           else return ()

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
     else get >>= parseError "Expecting a month." 

day :: TokenParser Integer
day = do
     n1 <- digit
     n2 <- digit
     if n1 >= 0 && n1 <= 9 && 
        n2 >= 1 && n2 <= 9
     then return $ digitsToInt [n1,n2]
     else get >>= parseError "Expecting a day." 

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
     else get >>= parseError "Expecting a year." 

digits :: TokenParser Integer
digits = do
     cs <- get
     case cs of
          (TokenDigit _:_) -> digitsAcc []
          _ -> parseError "Expecting digits." cs 
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
          _ -> parseError "Digit expected." cs

backslash :: TokenParser ()
backslash = do
     cs <- get
     case cs of
          (TokenBackslash:cs') -> do
               put cs'
               return ()
          _ -> parseError "Backslash expected." cs

colon :: TokenParser ()
colon = do
     cs <- get
     case cs of
          (TokenColon:cs') -> do
               put cs'
               return ()
          _ -> parseError "Colon expected." cs

none :: TokenParser ()
none = do
     cs <- get
     case cs of
          (TokenNone:cs') -> do
               put cs'
               return ()
          _ -> parseError "none expected." cs

newline :: TokenParser ()
newline = do
     spaces
     cs <- get
     case cs of
          (TokenNewline:cs') -> do
               put cs'
               return ()
          _ -> parseError "newline expected." cs

space :: TokenParser ()
space = do
     cs <- get
     case cs of
          (TokenSpace:cs') -> do
               put cs'
               return ()
          _ -> parseError "space expected." cs

trainingDay :: TokenParser ()
trainingDay = do
     cs <- get
     case cs of
          (TokenTrainingDay:cs') -> do
               put cs'
               return ()
          _ -> parseError "TrainingDay expected." cs

block :: TokenParser ()
block = do
     cs <- get
     case cs of
          (TokenBlock:cs') -> do
               put cs'
               return ()
          _ -> parseError "block expected." cs

sets :: TokenParser ()
sets = do
     cs <- get
     case cs of
          (TokenSets:cs') -> do
               put cs'
               return ()
          _ -> parseError "sets expected." cs

iteration :: TokenParser ()
iteration = do
     cs <- get
     case cs of
          (TokenIteration:cs') -> do
               put cs'
               return ()
          _ -> parseError "iteration expected." cs

measure :: TokenParser ()
measure = do
     cs <- get
     case cs of
          (TokenMeasure:cs') -> do
               put cs'
               return ()
          _ -> parseError "measure expected." cs

notes :: TokenParser ()
notes = do
     cs <- get
     case cs of
          (TokenNotes:cs') -> do
               put cs'
               return ()
          _ -> parseError "notes expected." cs

string :: TokenParser String
string = do
     cs <- get
     case cs of
          (TokenString s:cs') -> do
               put cs'
               return s
          _ -> parseError "string expected." cs

movements :: TokenParser ()
movements = do
     cs <- get
     case cs of
          (TokenMovements:cs') -> do
               put cs'
               return ()
          _ -> parseError "movements expected." cs

movement :: TokenParser ()
movement = do
     cs <- get
     case cs of
          (TokenMovement:cs') -> do
               put cs'
               return ()
          _ -> parseError "movement expected." cs

labels :: TokenParser ()
labels = do
     cs <- get
     case cs of
          (TokenLabels:cs') -> do
               put cs'
               return ()
          _ -> parseError "labels expected." cs

targets :: TokenParser ()
targets = do
     cs <- get
     case cs of
          (TokenTargets:cs') -> do
               put cs'
               return ()
          _ -> parseError "notes expected." cs

reps :: TokenParser ()
reps = do
     cs <- get
     case cs of
          (TokenReps:cs') -> do
               put cs'
               return ()
          _ -> parseError "reps expected." cs

measures :: TokenParser ()
measures = do
     cs <- get
     case cs of
          (TokenMeasures:cs') -> do
               put cs'
               return ()
          _ -> parseError "measures expected." cs

weight :: TokenParser ()
weight = do
     cs <- get
     case cs of
          (TokenWeight:cs') -> do
               put cs'
               return ()
          _ -> parseError "weight expected." cs

submovements :: TokenParser ()
submovements = do
     cs <- get
     case cs of
          (TokenSubmovements:cs') -> do
               put cs'
               return ()
          _ -> parseError "submovements expected." cs

dash :: TokenParser ()
dash = do
     cs <- get
     case cs of
          (TokenDash:cs') -> do
               put cs'
               return ()
          _ -> parseError "dash expected." cs

data Token = TokenDash
     | TokenBackslash 
     | TokenColon 
     | TokenNone 
     | TokenNewline
     | TokenSpace
     | TokenDigit Integer
     | TokenTrainingDay
     | TokenBlock
     | TokenSets
     | TokenIteration
     | TokenMeasure
     | TokenNotes
     | TokenString String
     | TokenMovements
     | TokenMovement
     | TokenLabels
     | TokenTargets
     | TokenReps
     | TokenScalers
     | TokenRPE
     | TokenMeasures
     | TokenWeight
     | TokenSubmovements
     deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
     | isDigit c = TokenDigit (read [c]) : lexer cs
lexer ('\t':cs) = TokenSpace : lexer cs
lexer (' ':cs) = TokenSpace : lexer cs
lexer ('\n':cs) = TokenNewline : lexer cs
lexer ('-':cs) = TokenDash : lexer cs
lexer ('"':cs) = lexString cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('/':cs) = TokenBackslash : lexer cs
lexer ('n':'o':'n':'e':cs) = TokenNone : lexer cs
lexer ('T':'r':'a':'i':'n':'i':'n':'g':'D':'a':'y':cs) = TokenTrainingDay : lexer cs
lexer ('b':'l':'o':'c':'k':cs) = TokenBlock : lexer cs
lexer ('s':'e':'t':'s':cs) = TokenSets : lexer cs
lexer ('s':'c':'a':'l':'e':'r':'s':cs) = TokenScalers : lexer cs
lexer ('i':'t':'e':'r':'a':'t':'i':'o':'n':cs) = TokenIteration : lexer cs
lexer ('m':'e':'a':'s':'u':'r':'e':'s':cs) = TokenMeasures : lexer cs
lexer ('m':'e':'a':'s':'u':'r':'e':cs) = TokenMeasure : lexer cs
lexer ('n':'o':'t':'e':'s':cs) = TokenNotes : lexer cs
lexer ('m':'o':'v':'e':'m':'e':'n':'t':'s':cs) = TokenMovements : lexer cs
lexer ('m':'o':'v':'e':'m':'e':'n':'t':cs) = TokenMovement : lexer cs
lexer ('l':'a':'b':'e':'l':'s':cs) = TokenLabels : lexer cs
lexer ('t':'a':'r':'g':'e':'t':'s':cs) = TokenTargets : lexer cs
lexer ('r':'e':'p':'s':cs) = TokenReps : lexer cs
lexer ('r':'p':'e':cs) = TokenRPE : lexer cs
lexer ('w':'e':'i':'g':'h':'t':cs) = TokenWeight : lexer cs
lexer ('s':'u':'b':'m':'o':'v':'e':'m':'e':'n':'t':'s':cs) = TokenSubmovements : lexer cs
lexer cs = error $ "Lexing Error: Uncrecognized symbol"++cs

lexString :: String -> [Token]
lexString cs = case r of
     [] -> [TokenString (clean s)]
     _ -> TokenString (clean s) : lexer (tail r)
     where 
          (s,r) = span (\c -> (c /= '"')) cs
          clean [] = []
          clean ('\n':' ':' ':cs) = clean ('\n':' ':cs)
          clean (c:cs) = c : clean cs

digitsToInt :: [Integer] -> Integer
digitsToInt [d] = d
digitsToInt ds = fst $ foldr (\d (r,i) -> (r + (d * (10^i)),i+1)) (0,0) ds

parseError :: String -> [Token] -> a
parseError msg ts = error $ "Parse error: " ++ msg ++ "\nRemaining Tokens: " ++ (show ts)