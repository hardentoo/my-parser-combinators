module Main.Parsing.Combinators
  ( module Main.Parsing.Parser
  , many
  , many1
  , lexeme
  , anyLexeme
  , string
  , oneOf
  , digit
  , natural
  , integer
  , spaces
  , try
  , endBy
  , endBy1
  , sepBy
  , sepBy1
  , foldr1P
  , foldl1P
  , between
  , brackets
  , parens
  , braces
  , angles
  ) where

import           Data.Char
import           Main.Parsing.Parser

lexeme :: Eq lex => lex -> Parser lex ()
lexeme l = () <$ satisfy (== l)

anyLexeme :: Parser lex lex
anyLexeme = satisfy (const True)

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

string :: Eq lex => [lex] -> Parser lex ()
string = foldr ((*>) . lexeme) (pure ())

oneOf :: Eq lex => [lex] -> Parser lex lex
oneOf = foldr (\lex -> (<|>) (lex <$ lexeme lex)) empty

many :: Parser lex a -> Parser lex [a]
many p = (:) <$> p <*> many p <|> pure []

many1 :: Parser lex a -> Parser lex [a]
many1 p = (:) <$> p <*> (many p <|> pure [])

natural :: Parser Char Integer
natural = read <$> many1 (satisfy isDigit)

integer :: Parser Char Integer
integer = natural <|> fmap negate (lexeme '-' *> natural)

spaces :: Parser Char ()
spaces = () <$ many (lexeme ' ')

try :: Parser lex a -> Parser lex (Maybe a)
try p = Just <$> p <|> pure Nothing

endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy p q = many (p <* q)

endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 p q = many1 (p <* q)

sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy p q = sepBy1 p q <|> pure []

sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 p q = (:) <$> p <*> many (q *> p)

between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between p q s = p *> s <* q

brackets :: Parser Char a -> Parser Char a
brackets = between (lexeme '[') (lexeme ']')

parens :: Parser Char a -> Parser Char a
parens = between (lexeme '(') (lexeme ')')

braces :: Parser Char a -> Parser Char a
braces = between (lexeme '{') (lexeme '}')

angles :: Parser Char a -> Parser Char a
angles = between (lexeme '<') (lexeme '>')

foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P f p q = f' <$> p <*> many ((,) <$> q <*> p)
  where
    f' acc []          = acc
    f' acc ((a, b):xs) = f acc a (f' b xs)

foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P f p q = f' <$> p <*> many ((,) <$> q <*> p)
  where
    f' acc []          = acc
    f' acc ((a, b):xs) = f' (f acc a b) xs
