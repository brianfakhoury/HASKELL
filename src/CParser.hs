module CParser where

import Ast
import ParserMonad
import TestsProject

parser :: Parser Program
parser = do fs <- some func
            return $ Program fs

func :: Parser Func
func = (do token $ literal "def"
           funcName <- varParser
           token $ literal "("
           paramName <- varParser
           token $ literal ")"
           blk <- block
           return $ Func funcName paramName blk) <||> (do token $ literal "def"
                                                          funcName <- varParser
                                                          token $ literal "()"
                                                          blk <- block
                                                          return $ Func funcName "" blk)

block :: Parser Block
block = block' <||> parseWhile <||> parseIf

block' :: Parser Block
block' = do token $ literal "{"
            blks <- some blocks
            token $ literal "}"
            return $ Blocks $ concat blks

blocks :: Parser [Block]
blocks = do stmts <- rep stmt
            blks <- rep block
            case (stmts, blks) of
                 ([],[]) -> failParse
                 _       -> return $ (Stmts stmts) : blks

parseWhile :: Parser Block
parseWhile = do token $ literal "while"
                token $ literal "("
                bx <- bexpr
                token $ literal ")"
                blk <- block
                return $ While bx blk

parseIf :: Parser Block
parseIf = (do token $ literal "if"
              token $ literal "("
              bx <- bexpr
              token $ literal ")"
              blk <- block
              token $ literal "else"
              blk2 <- block
              return $ IfElse bx blk blk2) <||> parseIf'

parseIf' :: Parser Block
parseIf' = do token $ literal "if"
              token $ literal "("
              bx <- bexpr
              token $ literal ")"
              blk <- block
              return $ If bx blk

stmt :: Parser Stmt
stmt = parseAssign <||> parseReturn <||> parsePrint <||> parseBreak <||> parseContinue

parseAssign :: Parser Stmt
parseAssign = do varName <- varParser
                 token $ literal "="
                 ex <- expr
                 token $ literal ";"
                 return $ Assignment varName ex

parseReturn :: Parser Stmt
parseReturn = do token $ literal "return"
                 ex <- expr
                 token $ literal ";"
                 return $ Return ex

parsePrint :: Parser Stmt
parsePrint = do token $ literal "print"
                id <- varParser
                token $ literal ";"
                return $ Print id

parseBreak :: Parser Stmt
parseBreak = do token $ literal "break"
                token $ literal ";"
                return Break

parseContinue :: Parser Stmt
parseContinue = do token $ literal "continue"
                   token $ literal ";"
                   return Continue

bexpr :: Parser BExpr
bexpr = withInfix bexpr' [("||", Or)]
bexpr' = withInfix bexpr'' [("&&", And)]
bexpr'' = (do token $ literal "!"
              bf <- bexpr
              return $ Not bf) <||> cond <||> bexpr

cond = parseEq <||> parseNeq <||> parseLt <||> parseLte <||> parseGt <||> parseGte

parseEq :: Parser BExpr
parseEq = do x1 <- expr
             token $ literal "=="
             x2 <- expr
             return $ Equal x1 x2

parseNeq :: Parser BExpr
parseNeq = do x1 <- expr
              token $ literal "!="
              x2 <- expr
              return $ NotEqual x1 x2

parseLt :: Parser BExpr
parseLt = do x1 <- expr
             token $ literal "<"
             x2 <- expr
             return $ Lt x1 x2

parseLte :: Parser BExpr
parseLte = do x1 <- expr
              token $ literal "<="
              x2 <- expr
              return $ Lte x1 x2

parseGt :: Parser BExpr
parseGt = do x1 <- expr
             token $ literal ">"
             x2 <- expr
             return $ Gt x1 x2

parseGte :: Parser BExpr
parseGte = do x1 <- expr
              token $ literal ">="
              x2 <- expr
              return $ Gte x1 x2

expr :: Parser Expr
expr = (do token $ literal "("
           res <- expr
           token $ literal ")"
           return res) <||> withInfix expr' [("+", Plus), ("-", Minus)]

expr' :: Parser Expr
expr' = withInfix expr'' [("*", Mult), ("/", Div), ("%", Mod)]

expr'' :: Parser Expr
expr'' = parseInt <||> identifier <||> (do token $ literal "("
                                           res <- expr
                                           token $ literal ")"
                                           return res)

parseInt :: Parser Expr
parseInt = (do i <- natParser
               return $ ValInt i) <||> (do token $ literal "-"
                                           i <- natParser
                                           return $ Neg $ ValInt i)

identifier :: Parser Expr
identifier = (do ident <- varParser
                 token $ literal "()"
                 return $ Call ident) <||> identifier' <||> identifier''

identifier' :: Parser Expr
identifier' = do ident <- varParser
                 token $ literal "("
                 x <- expr
                 token $ literal ")"
                 return $ CallParams ident x


identifier'' :: Parser Expr
identifier'' = (do ident <- varParser
                   return $ Id ident) <||> (do token $ literal "-"
                                               ident <- varParser
                                               return $ Neg $ Id ident)
