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

-- lexeme l принимает лексему, если она равна l, иначе завершается с неуспехом.
lexeme :: Eq lex => lex -> Parser lex ()
lexeme l = () <$ satisfy (== l)

-- r <$ p -- это то же самое, что и fmap (\_ -> r) p, то есть выполнить p,
-- отбросить результат и вернуть r. anyLexeme принимает любую лексему.
anyLexeme :: Parser lex lex
anyLexeme = satisfy (const True)

-- digit принимает символ если он является цифрой.
digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

-- string lexs принимает последовательность лексем, если она совпадает с lexs,
-- иначе завершается с неуспехом.
string :: Eq lex => [lex] -> Parser lex ()
string = foldr ((*>) . lexeme) (pure ())

-- oneOf lexs принимает лексему l, если она есть в списке lexs, иначе
-- завершается с неуспехом.
oneOf :: Eq lex => [lex] -> Parser lex lex
oneOf = foldr (\lex -> (<|>) (lex <$ lexeme lex)) empty

-- many p парсит 0 и более вхождений p.
many :: Parser lex a -> Parser lex [a]
many p = (:) <$> p <*> many p <|> pure []

-- У <|> приоритет ниже, чем у <$>, <*>, <$ и прочих подобных операторов. many1
-- p парсит 1 и более вхождений p.
many1 :: Parser lex a -> Parser lex [a]
many1 p = (:) <$> p <*> (many p <|> pure [])

-- natural парсит натуральное число.
natural :: Parser Char Integer
natural = read <$> many1 (satisfy isDigit)

-- integer парсит целое число.
integer :: Parser Char Integer
integer = natural <|> fmap negate (lexeme '-' *> natural)

-- p *> q -- парсит последовательность p q и возвращает результат q. spaces
-- парсит 0 и более пробельных символов.
spaces :: Parser Char ()
spaces = () <$ many (lexeme ' ')

-- try p парсит 0 или 1 вхождение p.
try :: Parser lex a -> Parser lex (Maybe a)
try p = Just <$> p <|> pure Nothing

-- endBy p q парсит последовательность вида p q p q ... p q и возвращает список
-- значений p.
endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy p q = many (p <* q)

-- endBy1 p q парсит не пустую последовательность вида p q p q ... p q и
-- возвращает список значений p.
endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 p q = many1 (p <* q)

-- sepBy p q парсит последовательность вида p q p q ... p и возвращает список
-- значений p.
sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy p q = sepBy1 p q <|> pure []

-- sepBy1 p q парсит не пустую последовательность вида p q p q ... p и
-- возвращает список значений p.
sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 p q = (:) <$> p <*> many (q *> p)

-- between p q s парсит последовательность p s q и возвращает значение s.
between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between p q s = p *> s <* q

-- brackets p парсит последовательность '[' p ']' и возвращает значение p.
brackets :: Parser Char a -> Parser Char a
brackets = between (lexeme '[') (lexeme ']')

-- parens p парсит последовательность '(' p ')' и возвращает значение p.
parens :: Parser Char a -> Parser Char a
parens = between (lexeme '(') (lexeme ')')

-- braces p парсит последовательность '{' p '}' и возвращает значение p.
braces :: Parser Char a -> Parser Char a
braces = between (lexeme '{') (lexeme '}')

-- angles p парсит последовательность '<' p '>' и возвращает значение p.
angles :: Parser Char a -> Parser Char a
angles = between (lexeme '<') (lexeme '>')

-- foldr1P f p q парсит такую же последовательность, что и sepBy, но сворачивает
-- результат при помощи правой свертки.
foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P f p q = f' <$> p <*> many ((,) <$> q <*> p)
  where
    f' acc []          = acc
    f' acc ((a, b):xs) = f acc a (f' b xs)

-- foldl1P f p q парсит такую же последовательность, что и sepBy, но сворачивает
-- результат при помощи левой свертки.
foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P f p q = f' <$> p <*> many ((,) <$> q <*> p)
  where
    f' acc []          = acc
    f' acc ((a, b):xs) = f' (f acc a b) xs
