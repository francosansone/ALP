import Parsing
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Expr = Num Int | BinOp Op Expr Expr deriving Show

data Op = Add | Mul | Min | Div deriving Show

--ejercicio 4

expr::Parser Int 
expr =do t <- term
         (do char '+'
             e <- expr
             return(t+e)
          <|> return t)
            
term::Parser Int 
term =do t <- factor
         (do char '*'
             e <- term
             return(t*e)
          <|> return t)
          
factor::Parser Int 
factor = do d <- digit
            return (digitToInt d)
         <|> do char '('
                e <- expr
                char ')'
                return e
                 
expr2::Parser Expr 
expr2 = do t <- term2
           (do char '+'
               e <- expr2
               return(BinOp Add t e)
               <|> (do char '-'
                       e <- expr2
                       return(BinOp Min t e)
               <|> return t))
             
term2::Parser Expr 
term2 =do t <- factor2
          (do char '*'
              e <- term2
              return(BinOp Mul t e)
              <|> (do char '/'
                      e <- expr2
                      return(BinOp Div t e)
              <|> return t))

factor2 :: Parser Expr                       
factor2 = do t <- digit
             return (Num (digitToInt t))
          <|> do char '('
                 e <- expr2
                 char ')'
                 return e
             
             
--ejercicio 3 
       
transformer :: Parser a -> Parser a
transformer p = (do char '('
                    x <- p
                    char ')'
                    return x)
                <|> p


--ejercicio 5

data Lis = Cons Val Lis | Empty deriving Show
data Val = Nume Int | Lyr Char deriving Show

charint :: Parser Lis
charint = do symbol "["
             x <- charint'
             return x

charint' :: Parser Lis --consultar como parsear [1,'a']
charint' = do y <- do x <- letter
                      return (Lyr x)
                    <|> do x <- digit
                           return (Nume (digitToInt x))
              symbol ","
              xs <- charint'
              return (Cons y xs)
           <|> 
           do y <- do --symbol '
                      x <- letter
                      --symbol '
                      return (Lyr x)
                    <|> do x <- digit
                           return (Nume (digitToInt x))
              symbol "]"
              return (Cons y Empty) 


--ejercicio 6

data Basetype = DInt | DChar | DFloat deriving Show
type Hasktype = [Basetype] 

hask :: Parser Hasktype
hask = sepBy (do{ t <- string "int"; return DInt}
              <|> do {t <- string "char"; return DChar}
                  <|> do {t <- string "float"; return DFloat})
             (do {symbol "->"}) 


--ejercicio 7

data Hasktype1 = HInt | HChar | Fun Hasktype1 Hasktype1 deriving Show

hasfun :: Parser Hasktype1
hasfun = (do t <- typ
             symbol "->"
             s <- hasfun
             return (Fun t s))
         <|>(do t <- typ
                return t)

typ :: Parser Hasktype1
typ = do{ t <- string "int"; return HInt}
        <|> do {t <- string "char"; return HChar}
            <|> do {symbol "("; t <- hasfun; symbol ")"; return t}


--ejercicio 8

exp1 :: Parser Int
exp1 = do x <- noResta
          f <- exp'
          return (f x)

exp' :: Parser (Int -> Int)
exp' = do symbol "-"
          y <- noResta
          g <- exp'
          return (\x -> g(x-y))
       <|> do return id

noResta :: Parser Int
noResta = do x <- ter
             f <- noResta'
             return (f x)

noResta' :: Parser (Int -> Int)
noResta' = do symbol "+"
              x <- ter
              g <- noResta'
              return (\y -> g(x+y))
           <|> do return id

ter :: Parser Int
ter = do y <- noDiv
         f <- ter'
         return (f y)

ter' :: Parser (Int -> Int)
ter' = do symbol "/"
          x <- noDiv
          g <- ter'
          return (\y -> g(div x y))
        <|> do return id

noDiv :: Parser Int
noDiv = do x <- facto
           f <- noDiv'
           return (f x)

noDiv' :: Parser (Int -> Int)
noDiv' = do symbol "*"
            y <- facto
            g <- noDiv'
            return (\x -> g(y*x))
         <|> do return id

facto :: Parser Int
facto = do x <- digit
           return (digitToInt x)
         <|> do symbol "("
                x <- exp1
                symbol ")"
                return x

--ejercicio 9
--modifico la gramatica para evitar la recursion a izquierda

data Declaration = Dec Type Declarator deriving Show
data Type = CInt | CChar | CFloat deriving Show
data Declarator = Puntero Declarator | A Directd deriving Show
data Directd = Id String | Paren Directd | Arreglo Directd [Int] deriving Show
 --supongo que identifier es un string

declaration :: Parser Declaration
declaration =   (do symbol "int" 
                    e <- declarator
                    symbol ";"
                    return (Dec CInt e))
                 <|> (do symbol "float" 
                         e <- declarator
                         symbol ";"
                         return (Dec CFloat e))
                <|> (do symbol "char"
                        e <- declarator
                        symbol ";"
                        return (Dec CChar e))

declarator :: Parser Declarator
declarator = (do symbol "*"
                 x <- declarator
                 return (Puntero x))
             <|>
             (do x <- direct_declarator
                 return (A x))

direct_declarator :: Parser Directd
direct_declarator = do x <- nocorchete
                       symbol "["
                       y <- corchete
                       return (Arreglo x y)
                    <|> do x <- nocorchete
                           return x

nocorchete :: Parser Directd
nocorchete = do symbol "("
                x <- direct_declarator
                symbol ")"
                return (Paren x)
            <|> do xs <- ident
                   return (Id xs)

corchete :: Parser [Int]
corchete = do n <- digit
              symbol "]"
              symbol "["
              n' <- corchete
              return ((digitToInt n) : n')
           <|> do n <- digit
                  symbol "]"
                  return [(digitToInt n)]

