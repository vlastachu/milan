module AST where

newtype Program = Program [Statement] deriving (Show)

data Statement =  Write Expr 
                | Assign String Expr -- Identifier is string. Probably bad idea
                | While Cond [Statement] 
                | If Cond [Statement] (Maybe [Statement])
                deriving (Eq, Show)

data Cond = Cond Expr Relation Expr deriving (Eq, Show)

data Relation = Eq | Ne | Lt | Gt | Le | Ge deriving (Eq, Enum)

instance Show Relation where   --not good for debug, but usefull
    show r = show $ fromEnum r

data Expr =   Identifier String
            | Constant Int
            | Read
            | Binary BinOperator Expr Expr
            | Unary UnOperator Expr
            | Paren Expr
            deriving (Eq, Show)

data UnOperator = Neg deriving (Eq, Show)

data BinOperator = Add | Sub | Div | Mult deriving (Eq, Show) 
--warning Add and ADD (vm comand) look similiar
