module CCompiler where

import Ast
import ICInterpreter
import CompileMonad
import CParser
import ParserMonad
import TestsProject
import Prelude hiding (lookup, print, map)
import Data.Map(Map, lookup, insert, empty, fromList, member, map, mapKeys, elems, keys)


compile :: Program -> IC_Program
compile p = implementTargets $ compileIrs (getIrs $ compileCount (compileProgram p) 0) 0

type TargetMap =  Map Int Int

implementTargets :: [IC_Instruction] -> [IC_Instruction]
implementTargets ls = let tmap = createTargetMap ls
                          (tmapFinal, ics) = removeHalts (tmap, ls) 3
                      in adjustTargets ics tmapFinal

adjustTargets :: [IC_Instruction] -> TargetMap -> [IC_Instruction]
adjustTargets ics tmap = let jumps = keys tmap
                             targets = elems tmap
                         in updateJumps ics jumps targets

updateJumps :: [IC_Instruction] -> [Int] -> [Int] -> [IC_Instruction]
updateJumps ics [] [] = ics
updateJumps ics (l1:l1s) (l2:l2s) = case ics !! l1 of
                                         (Jump' int) -> updateJumps (replaceInst ics (Jump' l2) l1) l1s l2s
                                         (Call' int) -> updateJumps (replaceInst ics (Call' l2) l1) l1s l2s
                                         (Bzero' op int) -> updateJumps (replaceInst ics (Bzero' op l2) l1) l1s l2s

replaceInst :: [IC_Instruction] -> IC_Instruction -> Int -> [IC_Instruction]
replaceInst ls inst i = case splitAt i ls of
                             (l1, l2) -> l1 ++ (inst : (tail l2))

removeHalts :: (TargetMap, [IC_Instruction]) -> Int -> (TargetMap, [IC_Instruction])
removeHalts (tmap, ls) i = if i < length ls
                              then case ls !! i of
                                        Halt' -> case splitAt i ls of
                                                        (l1,l2) -> let newTmap = updateTMap tmap i
                                                                   in removeHalts (newTmap, l1 ++ (tail l2)) i
                                        _ -> removeHalts (tmap, ls) (i+1)
                              else (tmap, ls)

updateTMap :: TargetMap -> Int -> TargetMap
updateTMap tmap i = decrementTargets (decrementJumps tmap i) i

decrementJumps :: TargetMap -> Int -> TargetMap
decrementJumps tmap i = mapKeys (\ line -> if line > i then line - 1 else line) tmap

decrementTargets :: TargetMap -> Int -> TargetMap
decrementTargets tmap i = map (\ line -> if line > i then line - 1 else line) tmap

createTargetMap :: [IC_Instruction] -> TargetMap
createTargetMap ls = case ls !! 1 of
                          (Call' int) -> createTargetMap' ls (insert 1 (targetInt ls int) empty) 2
                          _ -> empty

createTargetMap' :: [IC_Instruction] -> TargetMap -> Int -> TargetMap
createTargetMap' ls tmap i = if i < length ls
                               then case ls !! i of
                                         (Call' int) -> createTargetMap' ls (insert i (targetInt ls int) tmap) (i+1)
                                         (Jump' int) -> createTargetMap' ls (insert i (targetInt ls int) tmap) (i+1)
                                         (Bzero' _ int) -> createTargetMap' ls (insert i (targetInt ls int) tmap) (i+1)
                                         _            -> createTargetMap' ls tmap (i+1)
                               else tmap

targetInt :: [IC_Instruction] -> Int -> Int
targetInt ls i = case ls !! i of
                      (Halt') -> targetInt ls (i+1)
                      _ -> i

-- i = intermediate program line number !!!!!! includes labels still
compileIrs :: [Ir] -> Int -> [IC_Instruction]
compileIrs [] _ = []
compileIrs irs i = if i < length irs
                      then case irs !! i of
                                (PreInstruction f) -> [compileIr (PreInstruction f) irs i] ++ (compileIrs irs (i+1))
                                _ -> [Halt'] ++ compileIrs irs (i+1)
                      else []


compileIr :: Ir -> [Ir] -> Int -> IC_Instruction
compileIr (PreInstruction f) irs i = icFromJust $ f (forwardB irs i) (backwardB irs i)
compileIr _ _ _ = Halt'

icFromJust :: Maybe IC_Instruction -> IC_Instruction
icFromJust (Just a) = a
icFromJust Nothing = Jump' (-50)

-- Compiler Proper
compileProgram :: Program -> CompileMonad ()
compileProgram (Program funcs) = do let ir0 = PreInstruction $ \ _ _ -> return $ Push'
                                    let ir1 = PreInstruction $ \ forward _ -> do line <- forward "func:main"
                                                                                 return $ Call' line
                                    let ir2 = PreInstruction $ \ _ _ -> return $ Halt'
                                    CompileMonad $ \x -> ((), [ir0, ir1, ir2], x)
                                    compileFuncs funcs

compileFuncs :: [Func] -> CompileMonad ()
compileFuncs ((Func s p b):funcs) = do label ("func:" ++ s)
                                       label ("param:" ++ p)
                                       compileFunc (Func s p b)
                                       compileFuncs funcs
compileFuncs [] = return ()

compileFunc :: Func -> CompileMonad ()
compileFunc (Func _ "" block) = compileBlock block
compileFunc (Func _ p block) = do let ir = PreInstruction $ \ _ _ -> return $ Assign' (Var' p) (Var' "_param")
                                  CompileMonad $ \x -> ((), [ir], x)
                                  compileBlock block


compileBlock :: Block -> CompileMonad ()
compileBlock (While e s) = do label "startWhile"
                              v <- resultOf $ compileBExpr e
                              jumpForwardIfZero v "endWhile"
                              compileBlock s
                              jumpBack "startWhile"
                              label "endWhile"
compileBlock (IfElse b t e) = do v <- resultOf $ compileBExpr b
                                 jumpForwardIfZero v "else"
                                 compileBlock t
                                 jumpForward "end if"
                                 label "else"
                                 compileBlock e
                                 label "end if"
compileBlock (If b t) = do v <- resultOf $ compileBExpr b
                           jumpForwardIfZero v "end if"
                           compileBlock t
                           label "end if"
compileBlock (Blocks (blk:blks)) = do compileBlock blk
                                      compileBlock (Blocks blks)
compileBlock (Blocks []) = return ()
compileBlock (Stmts (stmt:stmts)) = do compileStmt stmt
                                       compileBlock (Stmts stmts)
compileBlock (Stmts []) = return ()

compileStmt :: Stmt -> CompileMonad ()
compileStmt (Assignment s x) = do v <- resultOf $ compileExpr x
                                  let ir = PreInstruction $ \ _ _ -> return $ Assign' (Var' s) (Var' v)
                                  CompileMonad $ \x -> ((), [ir], x)
compileStmt (Return x) = do v <- resultOf $ compileExpr x
                            let ir = PreInstruction $ \ _ _ -> return $ Return' (Var' v)
                            CompileMonad $ \x -> ((), [ir], x)
compileStmt (Print s) = let ir = PreInstruction $ \ _ _ -> return $ Print' (s ++ " = ") (Var' s)
                        in CompileMonad $ \x -> ((), [ir], x)
compileStmt Break = jumpForward "endWhile"
compileStmt Continue = jumpBack "startWhile"

compileBExpr :: BExpr -> CompileMonad ()
compileBExpr (Or bx1 bx2) = do v1 <- resultOf $ compileBExpr bx1
                               v2 <- resultOf $ compileBExpr bx2
                               varName <- freshVar
                               label ("comp result:" ++ varName)
                               let ir = PreInstruction $ \ _ _ -> return $ Or' (Var' varName) (Var' v1) (Var' v2)
                               CompileMonad $ \x -> ((), [ir], x)
compileBExpr (And bx1 bx2) = do v1 <- resultOf $ compileBExpr bx1
                                v2 <- resultOf $ compileBExpr bx2
                                varName <- freshVar
                                label ("comp result:" ++ varName)
                                let ir = PreInstruction $ \ _ _ -> return $ And' (Var' varName) (Var' v1) (Var' v2)
                                CompileMonad $ \x -> ((), [ir], x)
compileBExpr (Not bx) = do v <- resultOf $ compileBExpr bx
                           varName <- freshVar
                           label ("comp result:" ++ varName)
                           let ir = PreInstruction $ \ _ _ -> return $ Not' (Var' varName) (Var' v)
                           CompileMonad $ \x -> ((), [ir], x)
compileBExpr (Equal x1 x2) = do v1 <- resultOf $ compileExpr x1
                                v2 <- resultOf $ compileExpr x2
                                varName <- freshVar
                                label ("comp result:" ++ varName)
                                let ir = PreInstruction $ \ _ _ -> return $ Equal' (Var' varName) (Var' v1) (Var' v2)
                                CompileMonad $ \x -> ((), [ir], x)
compileBExpr (NotEqual x1 x2) = do v1 <- resultOf $ compileExpr x1
                                   v2 <- resultOf $ compileExpr x2
                                   varName <- freshVar
                                   label ("comp result:" ++ varName)
                                   let ir = PreInstruction $ \ _ _ -> return $ NotEq' (Var' varName) (Var' v1) (Var' v2)
                                   CompileMonad $ \x -> ((), [ir], x)
compileBExpr (Lt x1 x2) = do v1 <- resultOf $ compileExpr x1
                             v2 <- resultOf $ compileExpr x2
                             varName <- freshVar
                             label ("comp result:" ++ varName)
                             let ir = PreInstruction $ \ _ _ -> return $ Lt' (Var' varName) (Var' v1) (Var' v2)
                             CompileMonad $ \x -> ((), [ir], x)
compileBExpr (Lte x1 x2) = do v1 <- resultOf $ compileExpr x1
                              v2 <- resultOf $ compileExpr x2
                              varName <- freshVar
                              label ("comp result:" ++ varName)
                              let ir = PreInstruction $ \ _ _ -> return $ Le' (Var' varName) (Var' v1) (Var' v2)
                              CompileMonad $ \x -> ((), [ir], x)
compileBExpr (Gt x1 x2) = do v1 <- resultOf $ compileExpr x1
                             v2 <- resultOf $ compileExpr x2
                             varName <- freshVar
                             label ("comp result:" ++ varName)
                             let ir = PreInstruction $ \ _ _ -> return $ Gt' (Var' varName) (Var' v1) (Var' v2)
                             CompileMonad $ \x -> ((), [ir], x)
compileBExpr (Gte x1 x2) = do v1 <- resultOf $ compileExpr x1
                              v2 <- resultOf $ compileExpr x2
                              varName <- freshVar
                              label ("comp result:" ++ varName)
                              let ir = PreInstruction $ \ _ _ -> return $ Ge' (Var' varName) (Var' v1) (Var' v2)
                              CompileMonad $ \x -> ((), [ir], x)

compileExpr :: Expr -> CompileMonad ()
compileExpr (Plus x1 x2) = do v1 <- resultOf $ compileExpr x1
                              v2 <- resultOf $ compileExpr x2
                              varName <- freshVar
                              label ("comp result:" ++ varName)
                              let ir = PreInstruction $ \ _ _ -> return $ Plus' (Var' varName) (Var' v1) (Var' v2)
                              CompileMonad $ \x -> ((), [ir], x)
compileExpr (Minus x1 x2) = do v1 <- resultOf $ compileExpr x1
                               v2 <- resultOf $ compileExpr x2
                               varName <- freshVar
                               label ("comp result:" ++ varName)
                               let ir = PreInstruction $ \ _ _ -> return $ Minus' (Var' varName) (Var' v1) (Var' v2)
                               CompileMonad $ \x -> ((), [ir], x)
compileExpr (Mult x1 x2) = do v1 <- resultOf $ compileExpr x1
                              v2 <- resultOf $ compileExpr x2
                              varName <- freshVar
                              label ("comp result:" ++ varName)
                              let ir = PreInstruction $ \ _ _ -> return $ Times' (Var' varName) (Var' v1) (Var' v2)
                              CompileMonad $ \x -> ((), [ir], x)
compileExpr (Div x1 x2) = do v1 <- resultOf $ compileExpr x1
                             v2 <- resultOf $ compileExpr x2
                             varName <- freshVar
                             label ("comp result:" ++ varName)
                             let ir = PreInstruction $ \ _ _ -> return $ Div' (Var' varName) (Var' v1) (Var' v2)
                             CompileMonad $ \x -> ((), [ir], x)
compileExpr (Mod x1 x2) = do v1 <- resultOf $ compileExpr x1
                             v2 <- resultOf $ compileExpr x2
                             varName <- freshVar
                             label ("comp result:" ++ varName)
                             let ir = PreInstruction $ \ _ _ -> return $ Mod' (Var' varName) (Var' v1) (Var' v2)
                             CompileMonad $ \x -> ((), [ir], x)
compileExpr (Neg x1) = do v1 <- resultOf $ compileExpr x1
                          varName <- freshVar
                          label ("comp result:" ++ varName)
                          let ir = PreInstruction $ \ _ _ -> return $ Uminus' (Var' varName) (Var' v1)
                          CompileMonad $ \x -> ((), [ir], x)
compileExpr (Call s) = do let ir1 = PreInstruction $ \ _ _ -> return $ Push'
                          let ir2 = PreInstruction $ \ _ backward -> do line <- backward ("func:" ++ s)
                                                                        return $ Call' line
                          assignVal <- freshVar
                          label ("comp result:" ++ assignVal)
                          let ir3 = PreInstruction $ \ _ _ -> return $ Assign' (Var' assignVal) (Var' "_ret_val")
                          CompileMonad $ \x -> ((), [ir1, ir2, ir3], x)
                          return ()
compileExpr (CallParams s x) = do v <- resultOf $ compileExpr x
                                  let ir1 = PreInstruction $ \ _ _ -> return $ Push'
                                  let ir2 = PreInstruction $ \ _ _ -> return $ Assign' (Var' "_param") (Var' v)
                                  let ir3 = PreInstruction $ \ forward backward -> do line1 <- backward ("func:" ++ s)
                                                                                      line2 <- forward ("func:" ++ s)
                                                                                      case (line1, line2) of
                                                                                           (-1, -1) -> return $ Call' (-1)
                                                                                           (-1, x)  -> return $ Call' x
                                                                                           (x, -1)  -> return $ Call' x
                                                                                           _        -> return $ Call' (-1)
                                  assignVal <- freshVar
                                  label ("comp result:" ++ assignVal)
                                  let ir4 = PreInstruction $ \ _ _ -> return $ Assign' (Var' assignVal) (Var' "_ret_val")
                                  CompileMonad $ \x -> ((), [ir1, ir2, ir3, ir4], x)
compileExpr (Id s) = return ()
compileExpr (ValInt i) = do name <- freshVar
                            label ("comp result:" ++ name)
                            let ir = PreInstruction $ \ _ _ -> return $ Assign' (Var' name) (Val' i)
                            CompileMonad $ \x -> ((), [ir], x)
