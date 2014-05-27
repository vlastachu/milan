module Compiler where
import Text.ParserCombinators.Parsec (ParseError)
import Data.Functor ((<$>))
import Data.List

import AST


data CompilerState = CompilerState { commands  :: [VMCommand], 
                                     errors    :: [String], 
                                     variables :: [(String, Int)] } --Map
                                     deriving (Eq, Show)

defCompilerState = CompilerState [] [] []

headOrDefault :: a -> [a] -> a -- probably built-in function
headOrDefault def (x:_) = x
headOrDefault def [] = def


addCommand :: VMCommand -> CompilerState -> CompilerState
addCommand command state = state {commands = command : commands state}

addCommands :: [VMCommand] -> CompilerState -> CompilerState
addCommands commands' state =  foldr addCommand state commands'

addError :: String -> CompilerState -> CompilerState
addError err state = state {errors = err : errors state}

getVar :: String -> CompilerState -> (CompilerState, Int)
getVar var state = case findVar var state of 
         Just i -> (state, i)
         Nothing ->(state {variables = (var, i') : variables state}, i') 
            where i' = 1 + snd (headOrDefault ("",0) $ variables state)

findVar :: String -> CompilerState -> Maybe Int
findVar var state = snd <$> find (\x -> fst x == var) (variables state)

data VMCommand =   NOP
                 | STOP
                 | LOAD Int
                 | STORE Int
                 | BLOAD Int
                 | BSTORE Int
                 | PUSH Int
                 | POP
                 | DUP
                 | ADD
                 | MULT
                 | SUB
                 | DIV
                 | INVERT
                 | COMPARE Relation
                 | JUMP Int
                 | JUMP_YES Int
                 | JUMP_NO Int
                 | INPUT
                 | PRINT
                 deriving (Eq, Show)


binaryOpToVMCommand Add   = ADD
binaryOpToVMCommand Sub   = SUB
binaryOpToVMCommand Mult  = MULT
binaryOpToVMCommand Div   = DIV

compileExpr :: Expr -> CompilerState -> CompilerState
compileExpr (Constant i) state     = addCommand (PUSH i) state
compileExpr Read state             = addCommand INPUT state
compileExpr (Binary op a b) state  = addCommand (binaryOpToVMCommand op) state''
        where state'  = compileExpr a state
              state'' = compileExpr b state'
compileExpr (Unary Neg e) state    = addCommand INVERT $ compileExpr e state
compileExpr (Paren e) state      = compileExpr e state
compileExpr (Identifier str) state = case findVar str state of 
                        Just i  -> addCommand (LOAD i) state
                        Nothing -> addError ("Variable " ++ str ++ " used but not defined") state

compileCond :: Cond -> CompilerState -> CompilerState
compileCond (Cond a rel b) state = addCommand (COMPARE rel) state''
        where state'  = compileExpr a state
              state'' = compileExpr b state'

compileStatements :: [Statement] -> CompilerState -> CompilerState
compileStatements statements state = foldr compileStatement state $ reverse statements

compileStatement :: Statement -> CompilerState -> CompilerState
compileStatement (Write e) state = addCommand PRINT $ compileExpr e state
compileStatement (Assign str e) state = addCommand (STORE varIndex) $ compileExpr e state'
        where (state', varIndex) = getVar str state

compileStatement (While cond statements) state = state4
            where state1 = compileCond cond state
                  state2 = addCommand (JUMP_NO $ length $ commands state4) state1 -- maybe + 1
                  state3 = compileStatements statements state2
                  state4 = addCommand (JUMP $ length $ commands state) state3

compileStatement (If cond statements elsest) state = state5
    where state1 = compileCond cond state
          state2 = addCommand (JUMP_NO $ length $ commands state4) state1
          state3 = compileStatements statements state2
          state4 = addCommand (JUMP $ length $ commands state5) state3
          state5 = case elsest of 
                        Nothing -> state4
                        Just else_statements   -> compileStatements else_statements state4
compile (Program statements) = compileStatements statements defCompilerState

-- show' :: Either ParseError CompilerState -> String
-- show' (Right state) = foldr (\x sum -> sum ++ show x ++ "\n") "" $ commands state
-- show' (Left error) = "error: " ++ show error

show' :: CompilerState -> String
show' state = foldr (\x sum -> sum ++ show x ++ "\n") "" $ commands state


