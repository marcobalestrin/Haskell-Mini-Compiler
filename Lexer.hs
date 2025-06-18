module Lexer where

import Data.Char

data Expr = BTrue
          | BFalse
          | Num Int
          | Paren Expr
          | Let String Expr Expr
          | Tuple Expr Expr
          | Fst Expr          
          | Snd Expr          
--Calculo Lambda
          | Var String
          | Lam String Ty Expr
          | App Expr Expr
--Operações Aritméticas
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
--Operações Lógicas
          | And Expr Expr
          | Or Expr Expr
          | Xor Expr Expr
          | Not Expr
--Operações Relacionais
          | GThan Expr Expr
          | LThan Expr Expr
          | Equal Expr Expr
          | If Expr Expr Expr
          deriving Show

data Ty = TBool
        | TNum
        | TFunc Ty Ty
        | TTuple Ty Ty
        deriving (Show, Eq)

data Tk = TkTrue
        | TkFalse
        | TkNum Int
        | TkOpen
        | TkClose
        | TkLParen
        | TkRParen
        | TkTNum
        | TkTBool
        | TkLet
        | TkAtr
        | TkIn
        | TkFst             
        | TkSnd             
        | TkComma
--Operações Aritméticas
        | TkAdd
        | TkSub
        | TkMult
--Operações Lógicas
        | TkAnd
        | TkOr
        | TkXor
        | TkNot
--Operações Relacionais
        | TkGThan
        | TkLThan
        | TkEqual
        | TkIf
        | TkThen
        | TkElse
--Calculo Lambda
        | TkVar String
        | TkLam
        | TkArrow
        | TkColon
        deriving Show

lexer :: String -> [Tk]
lexer [] = []
--Calculo Lambda
lexer ('\\':xs) = TkLam : lexer xs
lexer ('-':'>':xs) = TkArrow : lexer xs
lexer (':':xs) = TkColon : lexer xs
--Operações Aritméticas
lexer ('+':xs) = TkAdd : lexer xs
lexer ('-':xs) = TkSub : lexer xs
lexer ('*':xs) = TkMult : lexer xs
--Operações Lógicas
lexer ('&':'&':xs) = TkAnd : lexer xs
lexer ('|':'|':xs) = TkOr : lexer xs
lexer ('^':'^':xs) = TkXor : lexer xs
lexer ('!':xs) = TkNot : lexer xs
--Operações Relacionais
lexer ('>':xs) = TkGThan : lexer xs
lexer ('<':xs) = TkLThan : lexer xs
lexer ('=':'=':xs) = TkEqual : lexer xs
--Resto
lexer ('=':xs) = TkAtr : lexer xs
lexer ('{':xs) = TkOpen : lexer xs
lexer ('}':xs) = TkClose : lexer xs
lexer ('(':xs) = TkLParen : lexer xs
lexer (')':xs) = TkRParen : lexer xs
lexer (',':xs) = TkComma : lexer xs
lexer (x:xs)
        | isSpace x = lexer xs
        | isDigit x = lexNum (x:xs)
        | isAlpha x = lexKW (x:xs)

lexNum :: String -> [Tk]
lexNum xs = case span isDigit xs of
              (num, rest) -> TkNum (read num) : lexer rest

lexKW :: String -> [Tk]
lexKW xs = case span isAlpha xs of
             ("true", rest) -> TkTrue : lexer rest
             ("false", rest) -> TkFalse : lexer rest
             ("if", rest) -> TkIf : lexer rest
             ("then", rest) -> TkThen : lexer rest
             ("else", rest) -> TkElse : lexer rest
             ("number", rest) -> TkTNum : lexer rest
             ("boolean", rest) -> TkTBool : lexer rest
             ("let", rest) -> TkLet : lexer rest  
             ("in", rest) -> TkIn : lexer rest  
             ("fst", rest) -> TkFst : lexer rest   
             ("snd", rest) -> TkSnd : lexer rest   
             (var,rest) -> TkVar var : lexer rest
