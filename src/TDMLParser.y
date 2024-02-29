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
    movements    { TokenMovements }
    movement     { TokenMovement }
    labels       { TokenLabels }
    targets      { TokenTargets }
    reps         { TokenReps }
    scalers      { TokenScalers }
    rpe          { TokenRPE }
    measures     { TokenMeasures }
    weight       { TokenWeight }
    submovments  { TokenSubmovements }

%%

TrainingDay
    : TrainingDayT ':' Spaces Date newline Blocks { TrainingDay $4 $6 }
    | TrainingDayT ':' Date newline Blocks        { TrainingDay $3 $5 }

Blocks : Block                { [$1] }
       | Block newline Blocks { $1:$3 }

Block : DashOption block ':' Spaces Digits Spaces newline 
        BlockIteration newline 
        BlockMeasure newline 
        Notes newline 
        BlockMovements { Block (digitsToInt $5) $8 $10 $12 $14  }

BlockIteration : space Spaces iteration ':' Spaces newline BlockIterator newline { $7 }
BlockIterator : space DashOption sets ':' Spaces Digits { Sets (digitsToInt $6) }

BlockMeasure : space Spaces measure ':' Spaces newline Spaces BlockMeasurer newline { $8 }
BlockMeasurer : none { NoBlockMeasure }

BlockMovements : DashOption movements ':' Spaces newline Movements { $6 }

Movements : Movement            { [$1]    }
          | Movement Movements  { $1 : $2 }

Movement : Spaces DashOption movement ':' Spaces string newline
             Notes  newline 
             labels ':' Spaces Labels newline
             targets ':' Spaces Targets newline
             iteration ':' newline Iteration newline
             scalers ':' Scalers newline
             measures ':' newline Measures newline 
             submovments ':' Submovements newline            
             { Movement $6 $8 $13 $18 $23 $27 $32 $36 }

Submovements : none       { [] }
             | Movements  { $1 } 

Measures : newline Measure { [$2] }
        | newline Measure newline Measures { $2 : $4 }

{- Fill in the rest of the Measure's -}
Measure : DashOption Spaces weight ':' space Spaces Digits { MeasureWeight (fromIntegral (digitsToInt $7)) }

{- Fill in the rest of the Scaler's. -}
Scalers : none                           { [] }
        | newline Scaler                 { [$2] }
        | newline Scaler newline Scalers { $2 : $4 }

Scaler : DashOption Spaces rpe ':' Spaces Range { ScaleRPE $6  }

Range : digit '-' digit       { ($1,$3) }
      | digit '-' digit digit { ($1,digitsToInt [$3,$4]) }

{- Need to fill in the rest of the Iterations.  -}
Iteration : DashOption Spaces reps ':' Spaces Digits { IterateByReps (digitsToInt $6) }

Labels : newline label                { [$2] }
       | newline label newline Labels { $2 : $4 }
       | none                         { [] }

label : DashOption Spaces string { $3 }

Targets : newline target                { [$2] }
       | newline target newline Targets { $2 : $4 }
       | none                           { [] }

target : DashOption Spaces string { $3 }


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
lexer ('m':'e':'a':'s':'u':'r':'e':'s':cs) = TokenMeasures : lexer cs
lexer ('m':'e':'a':'s':'u':'r':'e':cs) = TokenMeasure : lexer cs
lexer ('n':'o':'t':'e':'s':cs) = TokenNotes : lexer cs
lexer ('m':'o':'v':'e':'m':'e':'n':'t':'s':cs) = TokenMovements : lexer cs
lexer ('m':'o':'v':'e':'m':'e':'n':'t':cs) = TokenMovement : lexer cs
lexer ('l':'a':'b':'e':'l':'s':cs) = TokenLabels : lexer cs
lexer ('t':'a':'r':'g':'e':'t':'s':cs) = TokenTargets : lexer cs
lexer ('r':'e':'p':'s':cs) = TokenReps : lexer cs
lexer ('w':'e':'i':'g':'h':'t':cs) = TokenWeight : lexer cs
lexer ('s':'u':'b':'m':'o':'v':'e':'m':'e':'n':'t':'s':cs) = TokenSubmovements : lexer cs

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