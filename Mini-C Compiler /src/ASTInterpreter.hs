module ASTInterpreter where

import Prelude hiding (lookup, print)
import Data.Map(Map, lookup, insert, empty, fromList, member)
import Ast
import StatefulUnsafeMonad
import CParser
import ParserMonad
import TestsProject

type Env = Map String Int

type GlobalEnv = Map String Func

type State = ([Env], GlobalEnv, [String]) -- local states, global state (symbol table for functions), print strings


getPrints :: State -> [String]
getPrints s = third s

getGlobal :: State -> GlobalEnv
getGlobal s = second s

getLocals :: State -> [Env]
getLocals s = first s

getLocal :: State -> Env
getLocal s = head $ getLocals s

third :: (a,b,c) -> c
third (_, _, x) = x

second :: (a,b,c) -> b
second (_, x, _) = x

first :: (a,b,c) -> a
first (x, _, _) = x


pushFrame :: StatefulUnsafe State ()
pushFrame = StatefulUnsafe $ \ s -> case getLocals s of
                                         [] -> (Ok (), ([empty], getGlobal s, getPrints s))
                                         l  -> (Ok (), (empty : l, getGlobal s, getPrints s))

popFrame :: StatefulUnsafe State ()
popFrame = StatefulUnsafe $ \ s -> case getLocals s of
                                        []     -> (Error "no local stacks left to pop", s)
                                        (l:ls) -> (Ok (), (ls, getGlobal s, getPrints s))

getLocalVal :: String -> StatefulUnsafe State Int
getLocalVal s = do state <- get
                   case lookup s $ getLocal state of
                        Just a -> return a
                        Nothing -> err ("no such local variable: " ++ s)

putLocalVal :: String -> Int -> StatefulUnsafe State ()
putLocalVal s i = StatefulUnsafe $ \state -> (Ok (), ((insert s i $ getLocal state) : (tail $ getLocals state), getGlobal state, getPrints state))

checkReturn :: StatefulUnsafe State Bool
checkReturn = do state <- get
                 return $ member "return" $ getLocal state


getGlobalFunc :: String -> StatefulUnsafe State Func
getGlobalFunc s = do state <- get
                     case lookup s $ getGlobal state of
                          Just a -> return a
                          Nothing -> err "could not find function in global scope"

putGlobalFunc :: String -> Func -> StatefulUnsafe State ()
putGlobalFunc s f = StatefulUnsafe $ \state -> (Ok (), (getLocals state, insert s f $ getGlobal state, getPrints state))

populateGlobalSymbols :: [Func] -> StatefulUnsafe State ()
populateGlobalSymbols funcs = case funcs of
                                   (Func name p ss):f:fs -> do populateGlobalSymbols (f:fs)
                                                               StatefulUnsafe $ \ s -> (Ok (), (getLocals s, insert name (Func name p ss) $ getGlobal s, getPrints s))
                                   [(Func name p ss)] -> StatefulUnsafe $ \ s -> (Ok (), (getLocals s, insert name (Func name p ss) $ getGlobal s, getPrints s))
                                   _ -> err "no functions found for global population"

print :: String -> StatefulUnsafe State ()
print s = StatefulUnsafe $ \ state -> (Ok (), (getLocals state, getGlobal state, (getPrints state) ++ [s]))




eval :: Program -> [String]
eval p = getPrints $ snd $ app (evalProgram p) ([], empty, [])

evalProgram :: Program -> StatefulUnsafe State Int
evalProgram (Program funcs) = do populateGlobalSymbols funcs
                                 main <- getGlobalFunc "main"
                                 pushFrame
                                 val <- evalFunc main
                                 popFrame
                                 return val

evalFunc :: Func ->  StatefulUnsafe State Int
evalFunc (Func funcName param blk) = do putLocalVal "break" 0
                                        putLocalVal "continue" 0
                                        evalBlock blk
                                        ret <- getLocalVal "return"
                                        return ret

evalStmt :: Stmt -> StatefulUnsafe State ()
evalStmt (Assignment s x) = do val <- evalExpr x
                               putLocalVal s val
evalStmt (Return x) = do val <- evalExpr x
                         putLocalVal "return" val
evalStmt (Print s) = do val <- getLocalVal s
                        print $ show val
evalStmt Break = putLocalVal "break" 1
evalStmt Continue = putLocalVal "continue" 1


evalBlock :: Block -> StatefulUnsafe State ()
evalBlock (While bx blk) = do b <- evalBExpr bx
                              br <- getLocalVal "break"
                              if (b && br == 0)
                                 then do evalBlock blk
                                         case br of
                                              0 -> evalBlock (While bx blk)
                                              _ -> do putLocalVal "break" 0
                                 else return ()
evalBlock (If bx blk) = do b <- evalBExpr bx
                           if b
                              then evalBlock blk
                              else return ()
evalBlock (IfElse bx blk1 blk2) = do b <- evalBExpr bx
                                     if b
                                        then evalBlock blk1
                                        else evalBlock blk2
evalBlock (Blocks (blk:blks)) = do evalBlock blk
                                   evalBlock (Blocks blks)
evalBlock (Blocks []) = return ()
evalBlock (Stmts (stmt:stmts)) = do r <- checkReturn
                                    if not r
                                       then do br <- getLocalVal "break"
                                               cont <- getLocalVal "continue"
                                               case (cont, br) of
                                                     (0,0) -> do evalStmt stmt
                                                                 evalBlock (Stmts stmts)
                                                     (1, _) -> do putLocalVal "continue" 0
                                                     _ -> return ()
                                       else return ()
evalBlock (Stmts []) = return ()

evalBExpr :: BExpr -> StatefulUnsafe State Bool
evalBExpr (Or bt bx) = do b1 <- evalBExpr bt
                          if b1
                             then return b1
                             else do b2 <- evalBExpr bx
                                     return b2
evalBExpr (And bf bt) = do b1 <- evalBExpr bf
                           if b1
                              then do b2 <- evalBExpr bt
                                      return b2
                              else return b1
evalBExpr (Not bf) = do b <- evalBExpr bf
                        return $ not b
evalBExpr (Equal x1 x2) = do val1 <- evalExpr x1
                             val2 <- evalExpr x2
                             return $ val1 == val2
evalBExpr (NotEqual x1 x2) = do val1 <- evalExpr x1
                                val2 <- evalExpr x2
                                return $ val1 /= val2
evalBExpr (Lt x1 x2) = do val1 <- evalExpr x1
                          val2 <- evalExpr x2
                          return $ val1 < val2
evalBExpr (Lte x1 x2) = do val1 <- evalExpr x1
                           val2 <- evalExpr x2
                           return $ val1 <= val2
evalBExpr (Gt x1 x2) = do val1 <- evalExpr x1
                          val2 <- evalExpr x2
                          return $ val1 > val2
evalBExpr (Gte x1 x2) = do val1 <- evalExpr x1
                           val2 <- evalExpr x2
                           return $ val1 >= val2

evalExpr :: Expr -> StatefulUnsafe State Int
evalExpr (Plus t x) = do val1 <- evalExpr t
                         val2 <- evalExpr x
                         return $ val1 + val2
evalExpr (Minus t x) = do val1 <- evalExpr t
                          val2 <- evalExpr x
                          return $ val1 - val2
evalExpr (Mult f t) = do val1 <- evalExpr f
                         val2 <- evalExpr t
                         return $ val1 * val2
evalExpr (Div f t) = do val1 <- evalExpr f
                        val2 <- evalExpr t
                        return $ val1 `quot` val2
evalExpr (Mod f t) = do val1 <- evalExpr f
                        val2 <- evalExpr t
                        return $ val1 `mod` val2
evalExpr (Neg f) = do num <- evalExpr f
                      return $ -num
evalExpr (Call s) = do func <- getGlobalFunc s
                       pushFrame
                       val <- evalFunc func
                       popFrame
                       return val
evalExpr (CallParams s x) = do func <- getGlobalFunc s
                               paramVal <- evalExpr x
                               pushFrame
                               case func of
                                    Func _ param _ -> putLocalVal param paramVal
                               val <- evalFunc func
                               popFrame
                               return val
evalExpr (Id s) = getLocalVal s
evalExpr (ValInt i) = return $ i
