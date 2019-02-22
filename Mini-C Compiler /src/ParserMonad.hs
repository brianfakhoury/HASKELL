module ParserMonad where
import Control.Monad(ap)
import Ast

import Data.Char

data Parser a = Parser (String -> Maybe (a, String))

parse :: Parser a -> (String -> Maybe (a, String))
parse (Parser f) = f

fromJust :: Maybe (Program, String) -> Program
fromJust (Just (a,_)) = a
fromJust Nothing = Program []

fromJust' :: Maybe (Expr, String) -> Expr
fromJust' (Just (a,_)) = a
fromJust' Nothing = ValInt 1

fromJust'' :: Maybe (BExpr, String) -> BExpr
fromJust'' (Just (a,_)) = a
fromJust'' Nothing = Lt (ValInt 1) (ValInt 1)

fromJust''' :: Maybe (Stmt, String) -> Stmt
fromJust''' (Just (a,_)) = a
fromJust''' Nothing = Continue

instance Functor Parser where
  fmap f (Parser pa) =  Parser $ \ x -> case pa x of
                                          Nothing        -> Nothing
                                          Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
  pure = return
  (<*>) = ap


instance Monad Parser where
  return a =  Parser $ \ x -> Just (a,x)
  (Parser pa) >>= f = Parser $ \ x ->  case pa x of
                                         Nothing       -> Nothing
                                         Just (a,rest) -> parse (f a) rest

(+++) :: Parser a -> Parser b -> Parser (a,b)
pa +++ pb =  undefined

item :: Parser Char
item = Parser $ \ input -> case input of ""    -> Nothing
                                         (h:t) -> Just (h, t)

failParse :: Parser a
failParse = Parser $ \ input -> Nothing

sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item
           if p c
           then return c
           else failParse

literal :: String -> Parser String
literal "" = return ""
literal (h:t) = do sat (==h)
                   literal t
                   return (h:t)

(<|>) :: Parser a -> Parser b -> Parser (Either a b)
parserA <|> parserB = Parser $ \ input ->  case parse parserA input of
                                                 Just (a, rest) -> Just (Left a, rest)
                                                 Nothing -> case parse parserB input of
                                                              Just (b, rest) -> Just (Right b, rest)
                                                              Nothing        -> Nothing

(<||>) :: Parser a -> Parser a -> Parser a
l <||> r = do res <- l <|> r
              case res of
               Left a -> return a
               Right a -> return a


some :: Parser a -> Parser ([a])
some pa = do a <- pa
             rest <- rep pa
             return (a:rest)

rep :: Parser a -> Parser ([a])
rep pa =  do res <- (some pa) <|> (return [])
             case res of Left ls  -> return ls
                         Right ls -> return ls

digit :: Parser Char
digit = sat isDigit

natParser :: Parser Int
natParser =  do digits <- some digit
                return $ read digits

intParser  :: Parser Int
intParser = do r <- (literal "-") <|> natParser
               case r of
                Left _ -> fmap (0-) natParser
                Right n -> return n

spaces :: Parser ()
spaces =  do rep (sat isSpace)
             return ()

token:: Parser a -> Parser a
token pa = do spaces
              a <- pa
              spaces
              return a

varParser :: Parser String
varParser = do chars <- some (sat (\x -> (isAlpha x) || (isDigit x)))
               return chars

oneOf :: [Parser a] -> Parser a
oneOf [] = failParse
oneOf (pa:rest) = pa <||> oneOf rest

withInfix :: Parser a -> [(String, a -> a -> a)] -> Parser a
withInfix pa ls = let operators = fmap fst ls
                      opParsers = fmap (\ s -> token $ literal s) operators
                      innerParser left = do s <- oneOf opParsers
                                            next <- pa
                                            case lookup s ls of
                                             Nothing -> failParse
                                             Just f ->  let out = f left next
                                                        in (innerParser out) <||> return out
                  in do l <- pa
                        (innerParser l) <||> (return l)
