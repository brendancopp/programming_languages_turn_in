--Structure

-- Lexer :: String -> {Lexeme}
-- parser :: [Lexeme] -> Program
-- interpreter :: Shore -> Program -> ? 

--Dependancies
--cabal update
--cabal install megaparsec

--Lexer
	data Lexeme
 = Key Keyword
 | Id String
 | LParen
 | RParen


 -- Parser
--Informally, a parser is a function that takes string and produces a value representing the structure of that string. 
--So naively, it is of type String -> t for some t. But looking under the hood, there is more to the situation.
 
import Text.Megaparsec -- the main combinator library
import Text.Megaparsec.Char -- various basic parsers for characters
import qualified Text.Megaparsec.Char.Lexer as L -- This avoids name clashes with Prelude.

type Parser = Parsec () String


-- space1 is a parser from Text.Megaparsec.Char that will consume one or more whitespaces
-- L.space is a combinator that combines its first argument to consume space characters and its 2nd and 3rd arguments to specify how comments are formed.
-- L.skipLineComment matches its 1st argument then ignores everything to end of line
-- L.skipblockComment matches begin- and end-comment strings and ignores everything between

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- Define a wrapper that consumes space after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer