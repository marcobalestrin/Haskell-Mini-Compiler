module Interpreter where

import Lexer

isValue :: Expr -> Bool
isValue BTrue = True 
isValue BFalse = True 
isValue (Num _) = True 
isValue (Lam _ _ _) = True 
isValue (Tuple e1 e2) = isValue e1 && isValue e2
isValue (Fst _) = False 
isValue (Snd _) = False 
isValue _ = False
 

subst :: String -> Expr -> Expr -> Expr
subst v0 e0 BTrue = BTrue
subst v0 e0 BFalse = BFalse
subst v0 e0 (Num n) = Num n
subst v e (Paren e1) = Paren (subst v e e1)
--Operações Aritméticas
subst v0 e0 (Add e1 e2) = Add (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (Sub e1 e2) = Sub (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (Mult e1 e2) = Mult (subst v0 e0 e1) (subst v0 e0 e2)
--Operações Lógicas
subst v0 e0 (And e1 e2) = And (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (Or e1 e2) = Or (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (Xor e1 e2) = Xor (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (Not e1) = Not (subst v0 e0 e1)
--Operações Relacionais
subst v0 e0 (GThan e1 e2) = GThan (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (LThan e1 e2) = LThan (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (Equal e1 e2) = Equal (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (If e1 e2 e3) = If (subst v0 e0 e1) (subst v0 e0 e2) (subst v0 e0 e3)
--Calculo Lambda
subst v0 e0 (Var v1) 
    | v0 == v1 = e0
    | otherwise = (Var v1)
subst v0 e0 (Lam v1 t0 e1) = Lam v1 t0 (subst v0 e0 e1)
subst v0 e0 (App e1 e2) = App (subst v0 e0 e1) (subst v0 e0 e2)
subst v0 e0 (Let v1 e1 e2) =
  Let v1 (subst v0 e0 e1)
         (if v0 == v1 then e2 else subst v0 e0 e2)
--Tuplas
subst v0 e0 (Tuple e1 e2) = Tuple (subst v0 e0 e1) (subst v0 e0 e2)
--First e Second
subst v0 e0 (Fst e) = Fst (subst v0 e0 e)   
subst v0 e0 (Snd e) = Snd (subst v0 e0 e)  

step :: Expr -> Expr
--Operações Aritméticas
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = Add (Num n1) (step e2)
step (Add e1 e2) = Add (step e1) e2
step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n1) e2) = Sub (Num n1) (step e2)
step (Sub e1 e2) = Sub (step e1) e2
step (Mult (Num n1) (Num n2)) = Num (n1 * n2)
step (Mult (Num n1) e2) = Mult (Num n1) (step e2)
step (Mult e1 e2) = Mult (step e1) e2
--Operações Lógicas
step (And BFalse _) = BFalse
step (And BTrue e2) = e2
step (And e1 e2) = And (step e1) e2
step (Or BTrue _) = BTrue
step (Or BFalse e2) = e2
step (Or e1 e2 ) = Or (step e1) e2
step (Xor BFalse e2) = e2
step (Xor BTrue e2) = Not e2
step (Xor e1 e2 ) = Xor (step e1) e2
step (Not BTrue) = BFalse
step (Not BFalse) = BTrue
step (Not e1) = Not (step e1)
--Operações Relacionais
step (GThan (Num n1) (Num n2)) 
    | n1 > n2 = BTrue
    | otherwise = BFalse
step (GThan (Num n1) e2) = GThan (Num n1) (step e2)
step (GThan e1 e2) = GThan (step e1) e2
step (LThan (Num n1) (Num n2)) 
    | n1 < n2 = BTrue
    | otherwise = BFalse
step (LThan (Num n1) e2) = LThan (Num n1) (step e2)
step (LThan e1 e2) = LThan (step e1) e2
step (Equal (Num n1) (Num n2)) 
    | n1 == n2 = BTrue
    | otherwise = BFalse
step (Equal (Num n1) e2) = Equal (Num n1) (step e2)
step (Equal e1 e2) = Equal (step e1) e2
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e0 e1 e2) = If (step e0) e1 e2
--Calculo Lambda
step (App e1@(Lam v0 t0 e0) e2)
    | isValue e2 = subst v0 e2 e0
    | otherwise = App e1 (step e2)
step (App e1 e2) = App (step e1) e2
--Let
step (Paren e) = e
step (Let v1 e1 e2)
    | isValue e1 = subst v1 e1 e2
    | otherwise = Let v1 (step e1) e2
--Tuplas e First e Second
step (Tuple e1 e2)
    | not (isValue e1) = Tuple (step e1) e2
    | not (isValue e2) = Tuple e1 (step e2)
    | otherwise = Tuple e1 e2
step (Fst e)
    | isValue e = case e of
                    Tuple v1 _ -> v1
                    _ -> error "Erro"
    | otherwise = Fst (step e)

step (Snd e)
    | isValue e = case e of
                    Tuple _ v2 -> v2
                    _ -> error "Erro"
    | otherwise = Snd (step e)

eval :: Expr -> Expr
eval e | isValue e = e
       | otherwise = eval (step e)