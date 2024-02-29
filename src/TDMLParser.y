{
module TDMLParser where
import DataModel
import Data.Char
}

%name tdmlParser
%tokentype { Token }
%error { parseError }

%token 
    '-'          { TokenDash }
    '/'          { TokenBackslash }
    ':'          { TokenColon }
    none         { TokenNone }
    newline      { TokenNewline }
    space        { TokenSpace }
    digit        { TokenDigit $$ }
    TrainingDayT { TokenTrainingDay }
    block        { TokenBlock }
    sets         { TokenSets }
    iteration    { TokenIteration }
    measure      { TokenMeasure }
    notes        { TokenNotes }
    string       { TokenString $$ }

%%

TrainingDay
    : TrainingDayT ':' Spaces Date newline Blocks { TrainingDay $4 [] }
    | TrainingDayT ':' Date newline Blocks        { TrainingDay $3 [] }

Blocks : block        { [$1] }
       | block Blocks { $1:$2 }

Block : DashOption block ':' Spaces Digits Spaces newline 
        BlockIteration newline 
        BlockMeasure newline 
        Notes newline { {- Block (digitsToInt $5) $8 $10 $12 -}  }

BlockIteration : space Spaces iteration ':' Spaces newline BlockIterator newline { $3 }
BlockIterator : space DashOption sets ':' Spaces Digits { Sets (digitsToInt $6) }

BlockMeasure : space Spaces measure ':' Spaces newline Spaces BlockMeasurer newline { $3 }
BlockMeasurer : none { NoBlockMeasure }

BlockMovements : DashOption movements ':' Spaces newline Movements { $5 }

Movements : Movement            { [$1]    }
          | Movement Movements  { $1 : $2 }

Movement : DashOption movement ':' Spaces string newline
             Notes  newline 
             {}

Notes : space Spaces notes ':' Spaces string  { $6 }

DashOption : Spaces '-' space {}

Spaces
    : space Spaces { TokenSpace }
    | space        { TokenSpace }

Date 
    : Month '/' Day '/' Year { Date $3 $1 $5 }

Month 
    : digit digit { if $1 >= 0 && $1 <= 9 && $2 >= 1 && $2 <= 9
                         then $1 * 10 + $2
                         else error "Invalid Month"}

Day 
    : digit digit { if $1 >= 0 && $1 <= 9 && $2 >= 1 && $2 <= 9
                         then $1 * 10 + $2
                         else error "Invalid Day"}

Year 
    : digit digit digit digit { if $1 >= 1 && $1 <= 9 && 
                                   $2 >= 0 && $2 <= 9 &&
                                   $3 >= 0 && $3 <= 9 &&
                                   $4 >= 0 && $2 <= 9
                         then $1 * 1000 + $2 * 100 + $3 * 10 + $4
                         else error "Invalid year"}

Digits : digit Digits { $1 : $2 }
       | digit { [$1] }   

{
data Token
    = TokenDash
    | TokenBackslash 
    | TokenColon 
    | TokenNone 
    | TokenNewline
    | TokenSpace
    | TokenDigit Int
    | TokenTrainingDay
    | TokenBlock
    | TokenSets
    | TokenIteration
    | TokenMeasure
    | TokenNotes
    | TokenString String
    deriving Show

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
lexer ('i':'t':'e':'r':'a':'t':'i':'o':'n':cs) = TokenIteration : lexer cs
lexer ('m':'e':'a':'s':'u':'r':'e':cs) = TokenMeasure : lexer cs
lexer ('n':'o':'t':'e':'s':cs) = TokenNotes : lexer cs

lexString :: String -> [Token]
lexString cs = TokenString s : lexer (tail r)
 where (s,r) = span (\c -> not (c == '"')) cs
       clean [] = []
       clean ('\n':' ':' ':cs) = '\n' : ' ' : clean cs

digitsToInt :: [Int] -> Int
digitsToInt [d] = d
digitsToInt ds = snd $ foldr (\d (r,i) -> (r + d * 10 * i,i+1)) (1,1) ds

parseError :: [Token] -> a
parseError ts = error $ "Parse error: " ++ (show ts)

testParser = print . tdmlParser . lexer      
}