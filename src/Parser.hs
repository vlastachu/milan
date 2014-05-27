module Parser where
import Text.ParserCombinators.Parsec
import Control.Applicative ((<*>), (*>), (<*), (<$>), pure)
import Data.Functor
import Text.Parsec.Expr
import Control.Monad (void)
import AST
import qualified Lexer as L


--Parser
program :: GenParser Char st Program
program = Program <$> (L.commentsOrSpaces *> (L.begin *> statements) <* (L.end <* eof))

statements :: GenParser Char st [Statement]
statements = statement `sepBy` L.semicolon

statement :: GenParser Char st  Statement
statement =  Write  <$> (L.write *> (L.parens expr))
         <|> While  <$> (L.while *> (cond <* L.do')) <*> (statements <* L.od)
         <|> If     <$> (L.if' *> cond) <*> (L.then' *> statements) 
                    <*> optionMaybe (L.else' *> statements) <* L.fi
         <|> Assign <$> L.ident <*> (L.assign *> expr)       
         <?> "statement"

cond :: GenParser Char st Cond
cond = Cond <$> expr <*> relation <*> expr

relation :: GenParser Char st Relation
relation = Eq <$ L.eq 
       <|> Ne <$ L.ne 
       <|> Ge <$ L.ge 
       <|> Le <$ L.le 
       <|> Gt <$ L.gt 
       <|> Lt <$ L.lt
       <?> "relation operator"

expr    = buildExpressionParser table term
          <?> "expression"

term    =   Paren    <$> L.parens expr 
	<|> Read     <$  L.read 
	<|> (Constant . (fromInteger::Integer -> Int))   <$> L.int
	<|> (Identifier) <$> L.ident
	<?> "term (i.e. paren, digit, identifier)"

table   = [ [prefix L.minus Neg]
	  , [binary L.asterisk Mult AssocLeft, binary L.slash Div AssocLeft ]
	  , [binary L.plus Add AssocLeft, binary L.minus Sub   AssocLeft ]
	  ]

binary  name fun assoc = Infix (name >> return (Binary fun)) assoc
prefix  name fun       = Prefix (name >> return (Unary fun))

--parse test. TODO: remove later
pt f = parse f ""

parseProgram = parse program