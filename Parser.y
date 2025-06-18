{
module Parser where

import Lexer
}

%name parser 
%tokentype { Tk } 
%error { parseError } 

%token 
    num             { TkNum $$ }
    true            { TkTrue }
    false           { TkFalse }
    '{'             { TkOpen }
    '}'             { TkClose }
    '('             { TkLParen }
    ')'             { TkRParen }
--Operações Aritméticas
    '+'             { TkAdd }
    '-'             { TkSub }
    '*'             { TkMult }
--Operações Lógicas
    "&&"            { TkAnd }
    "||"            { TkOr }
    "^^"            { TkXor }
    '!'             { TkNot }
--Operações Relacionais
    '>'             { TkGThan }
    '<'             { TkLThan }
    "=="            { TkEqual }
    if              { TkIf }
    then            { TkThen }
    else            { TkElse }
    let             { TkLet  }
    in              { TkIn  }

    '\\'            { TkLam }
    ':'             { TkColon }
    "->"            { TkArrow }
    var             { TkVar $$ }
    number          { TkTNum }
    boolean         { TkTBool }
    '='             { TkAtr }
    ','             { TkComma }
    fst           { TkFst }
    snd           { TkSnd }


%nonassoc if then else var number boolean let in
%nonassoc '\\' "->" '(' ')' ':' '='
%left '+' '-'
%left '*' '/' '%'
%left "&&" "||" "^^"
%right '!'

%%

Exp     : num           { Num $1 }
        | '(' num ')'   { Num $2 }
        | true          { BTrue }
        | false         { BFalse }
--Operações Aritméticas
        | Exp '+' Exp   { Add $1 $3 }
        | Exp '-' Exp   { Sub $1 $3 }
        | Exp '*' Exp   { Mult $1 $3 }
        | '(' Exp '+' Exp ')'   { Add $2 $4 }
        | '(' Exp '-' Exp ')'   { Sub $2 $4 }
        | '(' Exp '*' Exp ')'   { Mult $2 $4 }
--Operações Lógicas
        | Exp "&&" Exp  { And $1 $3 }
        | Exp "||" Exp  { Or $1 $3 }
        | Exp "^^" Exp  { Xor $1 $3 }
        | '!' Exp       { Not $2 }
--Operações Relacionais
        | Exp '>' Exp   { GThan $1 $3 }
        | Exp '<' Exp   { LThan $1 $3 }
        | Exp "==" Exp  { Equal $1 $3 }
        | if '{' Exp '}' then '{' Exp '}' else '{' Exp '}'  { If $3 $7 $11 }
--Tuplas
        | '(' ExprList ')'           { nTuple $2 }
        | fst '(' Exp ')'            { Fst $3 }  
        | snd '(' Exp ')'            { Snd $3 }  
--Lambda
        | '\\' var ':' Type "->" Exp { Lam $2 $4 $6 }

        | Exp Exp                    { App $1 $2 }
--Parenteses
        | '(' Exp ')'                { Paren $2 }
--Let
        | let var '=' Exp in Exp     { Let $2 $4 $6}
--Var
        | var                        { Var $1  }
        
ExprList :: { [Expr] }
    : Exp                           { [$1] }
    | Exp ',' ExprList      { $1 : $3 }


Type    : boolean                    { TBool }
        | number                     { TNum }
        | '(' Type "->" Type ')'     { TFunc $2 $4 }

{

parseError :: [Tk] -> a 
parseError _ = error "Erro sintático"

--- Função que converte uma lista de expressões em pares aninhados
nTuple :: [Expr] -> Expr
nTuple [e] = e 
nTuple [e1, e2] = Tuple e1 e2
nTuple (e1:es) = Tuple e1 (nTuple es)
nTuple [] = error "Tupla vazia"
}