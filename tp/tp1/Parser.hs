module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import AST
import Data.Char

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do 
                  whiteSpace lis
                  t <- p
                  eof
                  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser (emptyDef   { commentStart  = "/*"
                                  , commentEnd    = "*/"
                                  , commentLine   = "//"
                                  , opLetter      = char '='
                                  , reservedNames = ["true","false","skip","if",
                                                     "then","else","end", "while","do"]
                                  })
  
----------------------------------
--- Parser de expressiones enteras
-----------------------------------

nat :: Parser IntExp
nat = do x <- natural lis 
         return (Const x)

var :: Parser IntExp
var = do x <- identifier lis
         return (Var x)


intexp :: Parser IntExp
intexp  = try (do char '-'
                  x <- intexp
                  return (UMinus x))
           <|> try (chainl1 term expr')
               <|> try (do x <- term
                           return x)
                   <|> (do b <- boolexp
                           symbol lis "?"
                           x <- intexp
                           symbol lis ":"
                           y <- intexp
                           return (Quest b x y ))

term :: Parser IntExp
term = try (chainl1 factor term')
       <|> (do x <- factor
               return x)

factor :: Parser IntExp
factor = try (do x <- nat 
                 return x)
             <|> try (do x <- var
                         return x)
                      <|> (do symbol lis "("
                              x <- intexp
                              symbol lis ")"
                              return x)
-----------------------------------
--- Parser auxiliar
------------------------------------

expr' :: Parser (IntExp -> IntExp -> IntExp)
expr' = try (do symbol lis "+"
                return Plus)
        <|> (do symbol lis "-"
                return Minus)
               
term' :: Parser (IntExp -> IntExp -> IntExp)
term' = try (do symbol lis "*"
                return Times)
            <|> (do symbol lis "/"
                    return Div) 

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

val :: Parser BoolExp
val = try (do symbol lis "True"
              return BTrue)
           <|> try (do symbol lis "False"
                       return BFalse)
                <|> (do symbol lis "("
                        b <- boolexp
                        symbol lis ")"
                        return b)

comp :: Parser BoolExp
comp = try (do x <- intexp
               symbol lis "="
               y <- intexp
               return (Eq x y))
           <|> try (do x <- intexp
                       symbol lis "<"
                       y <- intexp
                       return (Lt x y))
           <|> try (do x <- intexp
                       symbol lis ">"
                       y <- intexp
                       return (Gt x y))
           <|> (do x <- val
                   return x)

noot :: Parser BoolExp
noot = try (do symbol lis "~"
               b <- boolexp
               return (Not b))
      <|> (do x <- comp
              return x)
              
andy :: Parser BoolExp
andy = try (chainl1 noot and')
       <|> (do n <- noot
               return n)

boolexp :: Parser BoolExp
boolexp = try (do symbol lis "("
                  b <- boolexp
                  symbol lis ")"
                  return b)
              <|> try (chainl1 andy or')
                  <|> (do a <- andy
                          return a)

-----------------------------------
--- Parser auxiliar
------------------------------------

and' :: Parser (BoolExp -> BoolExp -> BoolExp)
and' =  (do symbol lis "&"
            return And)
                
or' :: Parser (BoolExp -> BoolExp -> BoolExp)
or' =  (do symbol lis "|"
           return Or)               

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = try (chainl1 comm1 comm')

comm1 :: Parser Comm
comm1 = try (do symbol lis "skip"
                return Skip)
        <|> try (do v <- var 
                    symbol lis ":="
                    i <- intexp
                    return (Let (aux v) i))
            <|> try (do symbol lis "if"
                        b <- boolexp
                        symbol lis "then"
                        c1 <- comm
                        symbol lis "else"
                        c2 <- comm
                        symbol lis "end"
                        return (Cond b c1 c2))
           <|> (do symbol lis "while"
                   b <- boolexp
                   symbol lis "do"   
                   c <- comm
                   symbol lis "end"  
                   return (While b c))


-----------------------------------
--- Parser auxiliar
------------------------------------

comm' :: Parser (Comm -> Comm -> Comm)
comm' = (do symbol lis ";"
            return Seq)

aux :: IntExp -> Variable
aux = (\(Var x) -> x)

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)