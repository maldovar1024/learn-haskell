module SExpr where

import AParser
import Control.Applicative
import Data.Char (isAlpha, isAlphaNum, isSpace)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> (oneOrMore p <|> pure [])

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show)

parseAtom = (N <$> posInt) <|> (I <$> ident)

parseComb = char '(' *> spaces *> oneOrMore (spaces *> parseSExpr) <* spaces <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = (A <$> parseAtom) <|> (Comb <$> parseComb)

testSrc = ["5", "foo3", "(bar (foo) 3 5 874)", "(((lambda x (lambda y (plus x y))) 3) 5)", "( lots of ( spaces in ) this ( one ) )"]

main = foldl (\m t -> m >> print (runParser parseSExpr t)) mempty testSrc
