module Ast where

data Program = Program [Func] deriving (Eq)

data Func = Func String String Block deriving (Eq)

data Block = While BExpr Block
           | If BExpr Block
           | IfElse BExpr Block Block
           | Blocks [Block]
           | Stmts [Stmt]
           deriving (Eq)

data Stmt = Assignment String Expr
          | Return Expr
          | Print String
          | Break
          | Continue
          deriving (Eq)

data BExpr = Or BExpr BExpr
           | And BExpr BExpr
           | Not BExpr
           | Equal Expr Expr
           | NotEqual Expr Expr
           | Lt Expr Expr
           | Lte Expr Expr
           | Gt Expr Expr
           | Gte Expr Expr
          deriving (Eq)

data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          | Neg Expr
          | Call String
          | CallParams String Expr
          | Id String
          | ValInt Int
          deriving (Eq)


instance Show Program where
    show (Program funcs) = foldr (\ func acc -> (show func) ++ acc) "" funcs

instance Show Func where
    show (Func name param block) = "\ndef " ++ name ++ "(" ++ param ++ ")" ++ " {" ++ (show block) ++ "\n}"

instance Show Block where
    show (While bx block) = "\nwhile (" ++ (show bx) ++ ")" ++ " {" ++ (show block) ++ "\n}"
    show (If bx block) = "\nif (" ++ (show bx) ++ ")" ++ " {" ++ (show block) ++ "\n}"
    show (IfElse bx blk1 blk2) = "\nif (" ++ (show bx) ++ ")" ++ " {" ++ (show blk1) ++ "\n}" ++ " else " ++ "{" ++ (show blk2) ++ "\n}"
    show (Blocks blks) = foldr (\ block acc -> (show block) ++ acc) "" blks
    show (Stmts stmts) = foldr (\ stmt acc -> (show stmt) ++ acc) "" stmts

instance Show Stmt where
    show (Assignment s x) = "\n" ++ s ++ " = " ++ (show x) ++ ";"
    show (Return x) = "\nreturn " ++ (show x) ++ ";"
    show (Print s) = "\n" ++ "print " ++ s ++ ";"
    show Break = "\nbreak;"
    show Continue = "\ncontinue;"

instance Show BExpr where
    show (Or bx1 bx2) = (show bx1) ++ " || " ++ (show bx2)
    show (And bx1 bx2) = (show bx1) ++ " && " ++ (show bx2)
    show (Not bx) = "!" ++ (show bx)
    show (Equal x1 x2) = (show x1) ++ " == " ++ (show x2)
    show (NotEqual x1 x2) = (show x1) ++ " != " ++ (show x2)
    show (Lt x1 x2) = (show x1) ++ " < " ++ (show x2)
    show (Lte x1 x2) = (show x1) ++ " <= " ++ (show x2)
    show (Gt x1 x2) = (show x1) ++ " > " ++ (show x2)
    show (Gte x1 x2) = (show x1) ++ " >= " ++ (show x2)

instance Show Expr where
    show (Plus x1 x2) = (show x1) ++ " + " ++ (show x2)
    show (Minus x1 x2) = (show x1) ++ " - " ++ (show x2)
    show (Mult x1 x2) = (show x1) ++ " * " ++ (show x2)
    show (Div x1 x2) = (show x1) ++ " / " ++ (show x2)
    show (Mod x1 x2) = (show x1) ++ " % " ++ (show x2)
    show (Neg x) = "-" ++ (show x)
    show (Call s) = s ++ "()"
    show (CallParams s x) = s ++ "(" ++ (show x) ++ ")"
    show (Id s) = s
    show (ValInt i) = show i
