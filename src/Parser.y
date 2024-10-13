{
module Parser where
import Structures
import Data.Char
}

%name calc
%tokentype { Token }
%error { parseError $$ }
%token
    '(' { TokenOB }
    ')' { TokenCB }
    '[' { TokenOS }
    ']' { TokenCS }
    'T' { TokenT }
    'F' { TokenF }
    'v' { TokenOr }
    '^' { TokenAnd }
    at  { TokenAtom $$ }
    '-' { TokenNeg }
    '{' { TokenOC }
    '}' { TokenCC }
    '=' { TokenEq }
    '>' { TokenRA }
    '<' { TokenLA }

%%
Struct : '(' AndStruct ')'                           { Conj $2 }
       | '[' OrStruct ']'                            { Disj $2 }
       | '-' '{' Struct '}'                          { Neg $3  }
       | '-' at                                      { Neg (Atom $2) }
       | '{' Struct '}' '=' '>' '{' Struct '}'       { Imp $2 $7 }
       | '{' Struct '}' '<' '=' '>' '{' Struct '}'   { Equ  $2 $8 }
       | at                                          { Atom $1 }
       | 'T'                                         { T }
       | 'F'                                         { F }

AndStruct : Struct                           { [$1] }
          | Struct '^' AndStruct             { $1 : $3 }

OrStruct : Struct                            { [$1] }
          | Struct 'v' OrStruct              { $1 : $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TokenOB  
    | TokenCB   
    | TokenOS   
    | TokenCS   
    | TokenT    
    | TokenF    
    | TokenOr   
    | TokenAnd  
    | TokenAtom String
    | TokenNeg  
    | TokenOC   
    | TokenCC   
    | TokenEq   
    | TokenRA   
    | TokenLA   
    deriving Show
           
lexer :: String -> [Token]
lexer [] = []
lexer ( '\n' : cs) = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
lexer ('(':cs)  = TokenOB : lexer cs
lexer (')':cs)  = TokenCB : lexer cs
lexer ('[':cs)  = TokenOS : lexer cs
lexer (']':cs)  = TokenCS : lexer cs
lexer ('^':cs)  = TokenAnd : lexer cs
lexer ('-':cs)  = TokenNeg : lexer cs
lexer ('{':cs)  = TokenOC : lexer cs
lexer ('}':cs)  = TokenCC : lexer cs
lexer ('=':cs)  = TokenEq : lexer cs
lexer ('>':cs)  = TokenRA : lexer cs
lexer ('<':cs)  = TokenLA : lexer cs
lexVar cs =
    case span isAlpha cs of
        ("T", rest)  -> TokenT : lexer rest
        ("F", rest)  -> TokenF : lexer rest
        ("v", rest)  -> TokenOr : lexer rest
        (at, rest)   -> TokenAtom at : lexer rest

parseStruct :: String -> Struct
parseStruct = calc . lexer
}

