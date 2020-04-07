module CompileMonad where
import Control.Monad(ap)
import ICInterpreter
import Data.List
import Data.Map(Map, lookup, insert, empty, fromList, member)

data CompileMonad a = CompileMonad (Integer -> (a,[Ir],Integer))

data Ir = PreInstruction ( (String -> Maybe Int) -> (String -> Maybe Int) -> Maybe IC_Instruction)
        | Label String
        | Scope [Ir]
instance Show Ir where
    show (Label x) = "Label: " ++ x
    show (Scope (l:ls)) = "Scope [" ++ (show l) ++ (show ls) ++ "]"
    show (Scope []) = ""
    show (PreInstruction x) = "PreInstruction: "

getIrs :: (a, [Ir], Integer) -> [Ir]
getIrs (_, l, _) = l

compileCount :: CompileMonad a -> (Integer -> (a,[Ir],Integer))
compileCount (CompileMonad t) = t

instance Functor CompileMonad where
  fmap f (CompileMonad a) =  CompileMonad $ \ int -> case a int of
                                                            (a, l, i) -> (f a, l, i)

instance Applicative CompileMonad where
  pure = return
  (<*>) = ap

instance Monad CompileMonad where
  return a = CompileMonad $ \ int -> (a, [], int)
  (CompileMonad ca) >>= f = CompileMonad $ \ int -> case ca int of
                                                         (a1, l1, i1) -> case compileCount (f a1) i1 of
                                                                              (a2, l2, i2) -> (a2, l1 ++ l2, i2)

label :: String -> CompileMonad ()
label s = CompileMonad $ \x -> ((), [Label s], x)

jumpForward :: String -> CompileMonad ()
jumpForward dest = let ir = PreInstruction $ \ forward _ -> do line <- forward dest
                                                               return $ Jump' line
                   in CompileMonad $ \ x -> ((), [ir], x)

jumpBack :: String -> CompileMonad ()
jumpBack dest = let ir = PreInstruction $ \ _ backward -> do line <- backward dest
                                                             return $ Jump' line
                in CompileMonad $ \ x -> ((), [ir], x)

jumpForwardIfZero :: String -> String -> CompileMonad ()
jumpForwardIfZero res dest = let ir = PreInstruction $ \ forward _ -> do line <- forward dest
                                                                         return $ Bzero' (Var' res) line
                             in CompileMonad $ \ x -> ((), [ir], x)

freshVar:: CompileMonad String
freshVar = do val <- getInt
              CompileMonad $ \x -> (("_t" ++ (show val)), [], x+1)

getInt :: CompileMonad Integer
getInt = CompileMonad $ \x -> (x, [], x)

resultOf :: CompileMonad a -> CompileMonad String
resultOf c = do c
                i <- getInt
                return $ ("_t" ++ (show (i-1)))

forwardB :: [Ir] -> Int -> String -> Maybe Int
forwardB ls i s = if i < length ls
                     then case ls !! i of
                               (Label str) -> if str == s
                                                 then Just i
                                                 else forwardB ls (i+1) s
                               _ -> forwardB ls (i+1) s
                     else Just 10000000

backwardB :: [Ir] -> Int -> String -> Maybe Int
backwardB ls i s = if i > 0
                      then case ls !! i of
                                (Label str) -> if str == s
                                                  then Just i
                                                  else backwardB ls (i-1) s
                                _ -> backwardB ls (i-1) s
                      else Just i
