module Eval1 (eval) where

import AST

-- Estados
type State = [(Variable,Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Int
lookfor v [] = error ("variable "++ v ++" no declarada")
lookfor v ((x,i):xs) | v == x       = i
                     | otherwise    = lookfor v xs
                               
-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Int -> State -> State
update v i []= [(v,i)]
update v i ((x,j):ys) | v == x      = ((v,i):ys)
                      | otherwise   = ((x,j):(update v i ys))
                              {-  (x:ys) -> case x of
                                                 (v, i) -> ((v,y):ys)
                                                  z -> (z : (update v y ys)))-}

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm = (\c xs   -> case c of
                            Skip            -> xs
                            Let v i         -> update v (evalIntExp i xs) xs
                            Seq c1 c2       -> evalComm c2 (evalComm c1 xs)
                            Cond b c1 c2    -> case evalBoolExp b xs of
                                                    True  -> evalComm c1 xs
                                                    False -> evalComm c2 xs                            
                            While b c1      -> case evalBoolExp b xs of
                                                    True  -> evalComm c (evalComm c1 xs)
                                                    False -> xs)
                                                    
                            
                            
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Int
evalIntExp i xs = case i of
                       Const x      -> fromInteger x
                       Var v        -> lookfor v xs
                       UMinus x     -> (*(-1)) (evalIntExp x xs)
                       Plus x y     -> (evalIntExp x xs) + (evalIntExp y xs)
                       Minus x y    -> (evalIntExp x xs) - (evalIntExp y xs)
                       Times x y    -> (evalIntExp x xs) * (evalIntExp y xs)
                       Div x y      -> div (evalIntExp x xs) (evalIntExp y xs)
                       Quest b x y  -> case b of
                                            BTrue   -> evalIntExp x xs
                                            _       -> evalIntExp y xs

    
-- Evalua una expresion booleana, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> State -> Bool
evalBoolExp b xs = case b of
                        BTrue       -> True
                        BFalse      -> False
                        Eq x y      -> (evalIntExp x xs) == (evalIntExp y xs)
                        Lt x y      -> (evalIntExp x xs) < (evalIntExp y xs)
                        Gt x y      -> (evalIntExp x xs) > (evalIntExp y xs)
                        And b0 b1   -> (evalBoolExp b0 xs) && (evalBoolExp b1 xs)
                        Or b0 b1    -> (evalBoolExp b0 xs) || (evalBoolExp b1 xs)
                        Not b       -> not (evalBoolExp b xs)












