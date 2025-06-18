module TypeChecker where

import Lexer

type Ctx = [(String,Ty)]

typeof :: Ctx -> Expr -> Maybe Ty
--Retorno Base
typeof ctx (Num _) = Just TNum
typeof ctx BTrue = Just TBool
typeof ctx BFalse = Just TBool
typeof ctx (Paren e) = typeof ctx e
--Operações Aritiméticas
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TNum, Just TNum) -> Just TNum
                       _                      -> Nothing
typeof ctx (Sub e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TNum, Just TNum) -> Just TNum
                       _                      -> Nothing
typeof ctx (Mult e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                        (Just TNum, Just TNum) -> Just TNum
                        _                      -> Nothing
--Operações Lógicas
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                      (Just TBool, Just TBool) -> Just TBool
                      _                        -> Nothing
typeof ctx (Xor e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                       (Just TBool, Just TBool) -> Just TBool
                       _                        -> Nothing
typeof ctx (Not e1) = case (typeof ctx e1) of
                    (Just TBool) -> Just TBool
                    _            -> Nothing
--Operações Relacionais
typeof ctx (GThan e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                         (Just TNum, Just TNum) -> Just TBool
                         _                      -> Nothing
typeof ctx (LThan e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                         (Just TNum, Just TNum) -> Just TBool
                         _                      -> Nothing
typeof ctx (Equal e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                         (Just TNum, Just TNum) -> Just TBool
                         _                      -> Nothing
typeof ctx (If e0 e1 e2) = case (typeof ctx e0, typeof ctx e1, typeof ctx e2) of
                         (Just TBool, t1, t2) | t1 == t2 -> t1
                                              | otherwise     -> Nothing
                         _ -> Nothing
--Calculo Lambda
typeof ctx (Var v0) = lookup v0 ctx 
typeof ctx (Lam v0 t0 e0) = case (typeof ((v0,t0) : ctx) e0) of 
                          Just t1 -> Just (TFunc t0 t1)
                          _ -> Nothing
typeof ctx (App e1 e2) = case (typeof ctx e1,typeof ctx e2) of
                       (Just (TFunc t0 t1), Just t2) | t0 == t2 -> Just t1
                                                     | otherwise -> Nothing 
                       _ -> Nothing
--Calculo let
typeof ctx (Let v0 e1 e2) = case (typeof ctx e1) of
                                Just t1 -> case (typeof ((v0,t1) : ctx) e2) of
                                           Just t2 -> Just t2
                                           _ -> Nothing
                                _ -> Nothing
--Calculo Tuplas
typeof ctx (Tuple e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                             (Just t1, Just t2) -> Just (TTuple t1 t2)
                             _                  -> Nothing
--Fst e Snd
typeof ctx (Fst e) = case typeof ctx e of
                        Just (TTuple t1 _) -> Just t1
                        _                  -> Nothing 
typeof ctx (Snd e) = case typeof ctx e of
                        Just (TTuple _ t2) -> Just t2
                        _                  -> Nothing 

typecheck :: Expr -> Expr
typecheck e = case typeof [] e of
             Just _ -> e
             _      -> error "Erro de Tipo" 