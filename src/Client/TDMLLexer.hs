{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Client.TDMLLexer (Token(..),
                         TokenData(..),
                         lexer,
                         getTokenData,
                         compareTokens,
                         digitsToInt) where
import Data.Char

data Token = TokenDash
     | TokenDot
     | TokenBackslash 
     | TokenColon 
     | TokenNone 
     | TokenCycle
     | TokenStart
     | TokenEnd
     | TokenNewline
     | TokenSpace
     | TokenDigit Integer
     | TokenTrainingDay
     | TokenBlock
     | TokenSubblock
     | TokenSets
     | TokenAmrap
     | TokenForTimeCap
     | TokenForTime
     | TokenIteration
     | TokenMeasure
     | TokenNotes
     | TokenJournal
     | TokenString String
     | TokenMovements
     | TokenMovement
     | TokenLabels
     | TokenTargets
     | TokenReps
     | TokenDistance
     | TokenScalers
     | TokenRPE
     | TokenMeasures
     | TokenWeight
     | TokenSubmovements
     | TokenTime 
     | TokenCalories
     | TokenIncreaseRoundsByReps
     deriving (Show,Eq)

compareTokens :: Token -> Token -> Bool
compareTokens (TokenString _) (TokenString _) = True
compareTokens (TokenDigit _) (TokenDigit _) = True
compareTokens t1 t2 = t1 == t2

data TokenData = StringData String 
     | IntegerData Integer
     | NoData

getTokenData :: Token -> TokenData
getTokenData (TokenDigit d) = IntegerData d
getTokenData (TokenString s) = StringData s
getTokenData _ = NoData

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
lexer ('.':cs) = TokenDot : lexer cs
lexer ('a':'m':'r':'a':'p':cs) = TokenAmrap : lexer cs
lexer ('b':'l':'o':'c':'k':cs) = TokenBlock : lexer cs
lexer ('c':'a':'l':'o':'r':'i':'e':'s':cs) = TokenCalories : lexer cs
lexer ('c':'y':'c':'l':'e':cs) = TokenCycle : lexer cs
lexer ('d':'i':'s':'t':'a':'n':'c':'e':cs) = TokenDistance : lexer cs
lexer ('e':'n':'d':cs) = TokenEnd : lexer cs
lexer ('f':'o':'r':'t':'i':'m':'e':'c':'a':'p':cs) = TokenForTimeCap : lexer cs
lexer ('f':'o':'r':'t':'i':'m':'e':cs) = TokenForTime : lexer cs
lexer ('j':'o':'u':'r':'n':'a':'l':cs) = TokenJournal : lexer cs
lexer ('n':'o':'n':'e':cs) = TokenNone : lexer cs
lexer ('T':'r':'a':'i':'n':'i':'n':'g':'D':'a':'y':cs) = TokenTrainingDay : lexer cs
lexer ('s':'e':'t':'s':cs) = TokenSets : lexer cs
lexer ('s':'c':'a':'l':'e':'r':'s':cs) = TokenScalers : lexer cs
lexer ('s':'t':'a':'r':'t':cs) = TokenStart : lexer cs
lexer ('s':'u':'b':'b':'l':'o':'c':'k':cs) = TokenSubblock : lexer cs
lexer ('i':'n':'c':'r':'e':'a':'s':'e':'-':'r':'o':'u':'n':'d':'s':'-':'b':'y':'-':'r':'e':'p':'s':cs) = TokenIncreaseRoundsByReps : lexer cs
lexer ('i':'t':'e':'r':'a':'t':'i':'o':'n':cs) = TokenIteration : lexer cs
lexer ('m':'e':'a':'s':'u':'r':'e':'s':cs) = TokenMeasures : lexer cs
lexer ('m':'e':'a':'s':'u':'r':'e':cs) = TokenMeasure : lexer cs
lexer ('n':'o':'t':'e':'s':cs) = TokenNotes : lexer cs
lexer ('m':'o':'v':'e':'m':'e':'n':'t':'s':cs) = TokenMovements : lexer cs
lexer ('m':'o':'v':'e':'m':'e':'n':'t':cs) = TokenMovement : lexer cs
lexer ('l':'a':'b':'e':'l':'s':cs) = TokenLabels : lexer cs
lexer ('t':'a':'r':'g':'e':'t':'s':cs) = TokenTargets : lexer cs
lexer ('t':'i':'m':'e':cs) = TokenTime : lexer cs
lexer ('r':'e':'p':'s':cs) = TokenReps : lexer cs
lexer ('r':'p':'e':cs) = TokenRPE : lexer cs
lexer ('w':'e':'i':'g':'h':'t':cs) = TokenWeight : lexer cs
lexer ('s':'u':'b':'m':'o':'v':'e':'m':'e':'n':'t':'s':cs) = TokenSubmovements : lexer cs
lexer cs = error $ "Lexing Error: Unrecognized symbol: "++cs

lexString :: String -> [Token]
lexString cs = case r of
     [] -> [TokenString (clean s)]
     _ -> TokenString (clean s) : lexer (tail r)
     where 
          (s,r) = span (\c -> (c /= '"')) cs
          clean [] = []
          clean ('\n':' ':' ':cs') = clean ('\n':' ':cs')
          clean (c:cs') = c : clean cs'

digitsToInt :: [Integer] -> Integer
digitsToInt [d] = d
digitsToInt ds = fst $ foldr (\d (r,i) -> (r + (d * (10^i)),i+1)) (0,0) ds