module Eval3 (eval) where

import AST

-- Estados
type State = [(Variable,Int)]

-- Estado nulo
initState :: State
initState = []

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Maybe Int
lookfor v [] = Nothing --error ("variable "++ v ++" no declarada")
lookfor v ((x,i):xs) | v == x       = Just i
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
eval :: Comm -> (State, Int)
eval p = (evalComm p initState, arit p initState)

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> State -> State
evalComm = (\c xs   -> case c of
                            Skip            -> xs
                            Let v i         -> case (evalIntExp i xs) of--update v (evalIntExp i xs) xs
                                                     Just x -> update v x xs
                                                     Nothing -> error ""
                            Seq c1 c2       -> evalComm c2 (evalComm c1 xs)
                            Cond b c1 c2    -> case evalBoolExp b xs of
                                                    True  -> evalComm c1 xs
                                                    False -> evalComm c2 xs                            
                            While b c1      -> case evalBoolExp b xs of
                                                    True  -> evalComm c (evalComm c1 xs)
                                                    False -> xs)
                            
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> State -> Maybe Int
evalIntExp i xs = case i of
                       Const x      -> Just (fromInteger x)
                       Var v        -> lookfor v xs
                       UMinus x     -> case (evalIntExp x xs) of    
                                            Just n  -> Just ((*(-1)) n)
                                            Nothing -> Nothing
                       Plus x y     -> case (evalIntExp y xs) of --(evalIntExp x xs) (evalIntExp y xs)
                                            Just n       -> case (evalIntExp x xs) of
                                                                 Just p -> Just (p + n)
                                                                 _      -> Nothing
                       Minus x y    -> case (evalIntExp y xs) of --(evalIntExp x xs) (evalIntExp y xs)
                                            Just n       -> case (evalIntExp x xs) of
                                                                 Just p -> Just (p - n)
                                                                 _      -> Nothing
                       Times x y    -> case (evalIntExp y xs) of --(evalIntExp x xs) (evalIntExp y xs)
                                            Just n       -> case (evalIntExp x xs) of
                                                                 Just p -> Just (p * n)
                                                                 _      -> Nothing
                       Div x y      -> case (evalIntExp y xs) of --(evalIntExp x xs) (evalIntExp y xs)
                                            Just 0       -> Nothing
                                            Just n       -> case (evalIntExp x xs) of
                                                                 Just p -> Just (div p n)
                                                                 _      -> Nothing
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



arit :: Comm -> State -> Int
arit Skip xs = 0
arit (Let x i) xs = arit' i
arit (Seq c0 c1) xs = (arit c0 xs) + (arit c1 xs)
arit (Cond b c1 c2) xs = case (evalBoolExp b xs) of
                                   True -> (aritb b) + arit c1 xs
                                   False -> (aritb b) + arit c2 xs
arit (While b c) xs = case (evalBoolExp b xs) of
                             True -> (aritb b) + (arit c xs)
                             False -> aritb b

aritb :: BoolExp -> Int
aritb BTrue = 0
aritb BFalse = 0
aritb (Eq i j) = (arit' i) + (arit' j)
aritb (Lt i j) = (arit' i) + (arit' j)
aritb (Gt i j) = (arit' i) + (arit' j)
aritb (And b0 b1) = (aritb b0) + (aritb b1)
aritb (Or b0 b1) = (aritb b0) + (aritb b1)
aritb (Not b) = aritb b

arit' :: IntExp -> Int
arit' (Const x) = 0
arit' (Var v) = 0
arit' (UMinus x) = 0
arit' (Plus x y) = 1 + (arit' x) + (arit' y)
arit' (Minus x y) = 1 + (arit' x) + (arit' y)
arit' (Times x y) = 1 + (arit' x) + (arit' y)
arit' (Div x y) = 1 + (arit' x) + (arit' y)









