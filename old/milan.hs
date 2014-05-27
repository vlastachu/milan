module Main where
import Text.ParserCombinators.Parsec
import Control.Applicative ((<*>), (*>), (<*), (<$>), pure)
import Data.Functor
import Data.List
import Control.Monad (void)

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
-- AST

newtype Program = Program { statements :: [Statement] } deriving (Eq, Show)

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
            | Binary Expr BinOperator Expr
            | Unary UnOperator Expr
            | Bracket Expr
            deriving (Eq, Show)

data UnOperator = Neg deriving (Eq, Show)

data BinOperator = Plus | Minus | Div | Mult deriving (Eq, Show)
tEndOfMultiLine :: GenParser Char st ()
tEndOfMultiLine = eof <|> (try (string "*/" >> return ())) <|> (anyChar >> tEndOfMultiLine)

tEndOfSingleLine :: GenParser Char st ()
tEndOfSingleLine = eof <|> (newline >> return ()) <|> (anyChar >> tEndOfSingleLine)
--Tokens
tCommentLine :: GenParser Char st ()
tCommentLine = do string "//"
                  tEndOfSingleLine
                  return () 

tCommentMultiLine :: GenParser Char st ()
tCommentMultiLine = do string "/*"
                       tEndOfMultiLine
                       return ()

tComment :: GenParser Char st ()
tComment =  try tCommentLine <|> tCommentMultiLine

tCommentsOrSpaces = many (tComment <|> (many1 space >> return ())) >> return () --warning

char_ c = do 
    a <- char c
    tCommentsOrSpaces
    return a

string_ str = do 
            a <- string str
            tCommentsOrSpaces
            return a

tBegin = string_ "begin"
tEnd   = string_ "end"
tIf    = string_ "if"
tThen  = string_ "then"
tElse  = string_ "else"
tFi    = string_ "fi"
tWhile = string_ "while"
tDo    = string_ "do"
tOd    = string_ "od"

tOBracket       = char_ '('
tCBracket       = char_ ')'
tAssign         = string_ ":="
tSemicolon      = char_ ';'
tRead           = string_ "read"
tWrite          = string_ "write"
tInt            = do i <- many1 digit
                     tCommentsOrSpaces
                     return i
tIdent          = (++) <$> many1 (letter <|> char_ '_')  <*> --TODO read about correct concatenate and remove many1
                  many (letter <|> digit <|> oneOf "_?!") --etc TODO add rules
--tBinary = oneOf "+-/*"
tPlus  = char_ '+'
tMinus = char_ '-'
tDiv   = char_ '/'
tMult  = char_ '*'
tUnary = char_ '-'

tEq = char_ '='
tNe = string_ "!="
tGt = char_ '>'
tLt = char_ '<'
tGe = string_ ">="
tLe = string_ "<="



--Parser
pProgram :: GenParser Char st Program
pProgram = Program <$> (tCommentsOrSpaces *> tBegin *> pStatements) <* tEnd <* eof

pStatements :: GenParser Char st [Statement]
pStatements = pStatement `sepBy` tSemicolon
--FIXME too much brackets
pStatement :: GenParser Char st  Statement
pStatement = try (Assign <$> tIdent <*> (tAssign *> pExpr))
         <|> Write  <$> (tWrite *> tOBracket *> pExpr <* tCBracket)
         <|> While  <$> (tWhile *> (pCond <* tDo)) <*> (pStatements <* tOd)
         <|> If     <$> (tIf *> pCond) <*> (tThen *> pStatements) 
                    <*> optionMaybe (tElse >> pStatements) <* tFi
         <?> "statement"

pCond :: GenParser Char st Cond
pCond = Cond <$> pExpr <*> pRelation <*> pExpr


pExpr :: GenParser Char st Expr
pExpr = try (Binary     <$> pExpr2 <*> pBinary <*> pExpr) --FIXME ready parsec operator parser
    <|> pExpr2
    <?> "expression"

pExpr2 :: GenParser Char st Expr
pExpr2 = try (Read  <$  tRead)
     <|> Constant   <$> ((read::String->Int) <$> tInt) --probably there is shorter way
     <|> Unary      <$> (Neg <$ tUnary) <*> pExpr --very bad
     <|> Bracket    <$> (tOBracket *> pExpr) <* tCBracket 
     <|> Identifier <$> tIdent

pRelation :: GenParser Char st Relation
pRelation = Eq <$ tEq 
        <|> Ne <$ tNe 
        <|> try (Ge <$ tGe) 
        <|> try (Le <$ tLe) 
        <|> Gt <$ tGt 
        <|> Lt <$ tLt
        <?> "relation operator"

pBinary :: GenParser Char st BinOperator
pBinary = Plus  <$ tPlus
     <|>  Minus <$ tMinus
     <|>  Mult  <$ tMult
     <|>  Div   <$ tDiv
     <?> "binary operator"

--parse test. TODO: remove later
pt f = parse f ""


-- compiler
-- 
priority :: BinOperator -> Int
priority Mult  = 2
priority Div   = 2
priority Plus  = 1
priority Minus = 1

correctOpOrder :: Expr -> Expr
correctOpOrder (Binary (Binary a op2 b) op1 c) 
        | priority op1 > priority op2 = Binary a' op2 (Binary b' op1 c')
        | otherwise                   = Binary (Binary a' op2 b') op1 c' 
        where [a', b', c'] = correctOpOrder <$> [a, b, c]
correctOpOrder (Binary a op1 (Binary b op2 c))
        | priority op1 > priority op2 = Binary (Binary a' op1 b') op2 c'
        | otherwise                   = Binary a' op1 (Binary b' op2 c')
        where [a', b', c'] = correctOpOrder <$> [a, b, c]
correctOpOrder (Unary op a) = Unary op $ correctOpOrder a
correctOpOrder (Bracket a) = Bracket $ correctOpOrder a
correctOpOrder other = other 


--correctExpr :: Expr -> Expr
--correctExpr = correctBracket . correctOpOrder

binaryOpToVMCommand Plus  = ADD
binaryOpToVMCommand Minus = SUB
binaryOpToVMCommand Mult  = MULT
binaryOpToVMCommand Div   = DIV

correctAndCompileExpr :: Expr -> CompilerState -> CompilerState
correctAndCompileExpr e state = compileExpr (correctOpOrder e) state

compileExpr :: Expr -> CompilerState -> CompilerState
compileExpr (Constant i) state     = addCommand (PUSH i) state
compileExpr Read state             = addCommand INPUT state
compileExpr (Binary a op b) state  = addCommand (binaryOpToVMCommand op) state''
        where state'  = compileExpr a state
              state'' = compileExpr b state'
compileExpr (Unary Neg e) state    = addCommand INVERT $ compileExpr e state
compileExpr (Bracket e) state      = compileExpr e state
compileExpr (Identifier str) state = case findVar str state of 
                        Just i  -> addCommand (LOAD i) state
                        Nothing -> addError ("Variable " ++ str ++ " used but not defined") state

compileCond :: Cond -> CompilerState -> CompilerState
compileCond (Cond a rel b) state = addCommand (COMPARE rel) state''
        where state'  = correctAndCompileExpr a state
              state'' = correctAndCompileExpr b state'

compileStatements :: [Statement] -> CompilerState -> CompilerState
compileStatements statements state = foldr compileStatement state $ reverse statements

compileStatement :: Statement -> CompilerState -> CompilerState
compileStatement (Write e) state = addCommand PRINT $ correctAndCompileExpr e state
compileStatement (Assign str e) state = addCommand (STORE varIndex) $ correctAndCompileExpr e state'
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

parseProgram = parse pProgram
compile (Program statements) = compileStatements statements defCompilerState

show' ::Either ParseError CompilerState -> String
show' (Right state) = foldr (\x sum -> sum ++ show x ++ "\n") "" $ commands state
show' (Left error) = "error: " ++ show error
--type CompilerState = [VMCommand],[Error],[variable] (Error == String, Variable = Map String => Int) 
main = undefined
