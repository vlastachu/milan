module Lexer where

import Prelude hiding (read)
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as T

--http://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Token.html#v:makeTokenParser

milanDef =T.LanguageDef{T.commentStart    = "/*",
			T.commentEnd      = "*/",
			T.commentLine     = "//",
			T.identStart      = (char '_' <|> letter) :: GenParser Char st Char, --это сложно понять
			T.identLetter     = (letter <|> char '_' <|> digit),
			T.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~",
			T.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~",
			T.reservedNames   = ["begin", "end", "if", "then", "else", "fi", "do", "while", "od", "read", "write"],
			T.reservedOpNames = [":=", "+", "-", "*", "/", "<", ">", "<=", ">=", "=", "!="],
			T.caseSensitive   = False,
			T.nestedComments  = False}

generatedLexer = T.makeTokenParser milanDef

lexer = generatedLexer
--{  
-- --you may change something there, but it seems useless 
--}

commentsOrSpaces = T.whiteSpace lexer

lexeme = T.lexeme lexer
reserved = (T.reserved lexer)
reservedOp = (T.reservedOp lexer)

begin = reserved "begin"
end   = reserved "end"
if'   = reserved "if"
then' = reserved "then"
else' = reserved "else"
fi    = reserved "fi"
while = reserved "while"
do'   = reserved "do"
od    = reserved "od"

read  = reserved "read"
write = reserved "write"

parens         = T.parens lexer
assign         = reservedOp ":="
semicolon      = T.semi lexer
int            = T.natural lexer
ident          = T.identifier lexer

plus     = reservedOp "+"
minus    = reservedOp "-"
slash    = reservedOp "/" --over, stroke
asterisk = reservedOp "*" --star?

eq = reservedOp "="
ne = reservedOp "!="
gt = reservedOp ">"
lt = reservedOp "<"
ge = reservedOp ">="
le = reservedOp "<="
