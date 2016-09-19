module Eval3 (eval) where

import AST

-- Estados
type State = [(Variable, Maybe Int)]

-- Estado nulo
initState :: (State, Int)
initState = ([], 0)

-- Busca el valor de una variabl en un estado
-- Completar la definicion
lookfor :: Variable -> State -> Maybe Int
lookfor v [] = Nothing 
lookfor v ((x,i):xs) | v == x       = i
                     | otherwise    = lookfor v xs
                               
-- Cambia el valor de una variable en un estado
-- Completar la definicion
update :: Variable -> Maybe Int -> State -> State
update v i []= [(v,i)]
update v i ((x,j):ys) | v == x      = ((v,i):ys)
                      | otherwise   = ((x,j):(update v i ys))
                             
-- Evalua un programa en el estado nulo
eval :: Comm -> (State, Int)
eval p = evalComm p initState

-- Evalua un comando en un estado dado
-- Completar definicion
evalComm :: Comm -> (State, Int) -> (State, Int)
evalComm = (\c (xs, n)   -> case c of
                              Skip            -> (xs, n)
                              Let v x         -> let (i,j) = evalIntExp x (xs,n)
                                                 in (update v i xs, j) 
                              Seq c1 c2       -> evalComm c2 (evalComm c1 (xs,n))
                              Cond b c1 c2    -> let (i, j) = evalBoolExp b (xs,n) 
                                                 in case i of
                                                       True  -> evalComm c1 (xs, j)
                                                       False -> evalComm c2 (xs ,j)                           
                              While b c1      -> let (i, j) = evalBoolExp b (xs,n) 
                                                 in case i of
                                                       True  -> evalComm c (evalComm c1 (xs,j))
                                                       False -> (xs,j))
                            
-- Evalua una expresion entera, sin efectos laterales
-- Completar definicion
evalIntExp :: IntExp -> (State, Int) -> (Maybe Int, Int)
evalIntExp i (xs, n) = case i of
                       Const x      -> (Just (fromInteger x), n)
                       Var v        -> (lookfor v xs, n)
                       UMinus x     -> case (evalIntExp x (xs,n)) of    
                                            (Just y, n')  -> (Just ((*(-1)) y), n')
                                            (Nothing, n') -> (Nothing, n')
                       Plus x y     -> case (evalIntExp y (xs,n)) of 
                                            (Just z, n')       -> case (evalIntExp x (xs,n')) of
                                                                 (Just p, n'') -> (Just (p + z), n'' + 1)
                                                                 (_, n'')      -> (Nothing, n'')
                       Minus x y    -> case (evalIntExp y (xs,n)) of 
                                            (Just z, n')       -> case (evalIntExp x (xs,n')) of
                                                                 (Just p, n'') -> (Just (p - z), n'' + 1)
                                                                 (_, n'')      -> (Nothing, n'')
                       Times x y    -> case (evalIntExp y (xs,n)) of 
                                            (Just z, n')       -> case (evalIntExp x (xs,n')) of
                                                                 (Just p, n'') -> (Just (p * z), n'' + 1)
                                                                 (_, n'')      -> (Nothing, n'')
                       Div x y      -> case (evalIntExp y (xs,n)) of 
                                            (Just 0, n')       ->( Nothing, n')
                                            (Just z, n')       -> case (evalIntExp x (xs,n')) of
                                                                    (Just p, n'') -> (Just (div p n), n'' + 1)
                                                                    (_, n'')      -> (Nothing,n'')


    
-- Evalua una expresion booleana, sin efectos laterales
-- Completar definicion
evalBoolExp :: BoolExp -> (State, Int) -> (Bool, Int)
evalBoolExp b (xs,n) = case b of
                        BTrue       -> (True, n)
                        BFalse      -> (False, n)
                        Eq x y      -> let ((i, j), (i', j')) = (evalIntExp x (xs,0), evalIntExp y (xs,0))
                                       in (i == i', n + j + j')
                        Lt x y      -> let ((i, j), (i', j')) = (evalIntExp x (xs,0), evalIntExp y (xs,0))
                                       in (i < i', j + j'+n)
                        Gt x y      -> let ((i, j), (i', j') )= (evalIntExp x (xs,0), evalIntExp y (xs,0))
                                       in (i > i', j + j'+n)
                        And b0 b1   -> let ((i, j), (i', j')) = (evalBoolExp b0 (xs,0), evalBoolExp b1 (xs,0))
                                       in (i && i', j + j'+n)
                        Or b0 b1    -> let ((i, j), (i', j')) = (evalBoolExp b0 (xs,0), evalBoolExp b1 (xs,0))
                                       in (i || i', j + j' + n)             
                        Not b       -> let (i,j) = (evalBoolExp b (xs,0))
                                       in (not i, j + n)





