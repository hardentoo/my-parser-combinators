module Main.Parsing.Parser
  ( Parser
  , pure
  , (<$>)
  , (<$)
  , (<*>)
  , (<*)
  , (*>)
  , empty
  , (<|>)
  , satisfy
  , eof
  , parse
  , runParser
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Tuple

newtype Parser lex a = Parser { unParser :: StateT [lex] Maybe a }

runParser :: Parser lex a -> [lex] -> Maybe ([lex], a)
runParser (Parser p) ls = swap <$> runStateT p ls

-- Если парсер p не поглащает весь список, то parse возвращает Nothing.
parse :: Parser lex a -> [lex] -> Maybe a
parse p ls = do
  (ls', a) <- runParser p ls
  case ls' of
    [] -> pure a
    _  -> Nothing

-- satisfy p принимает лексему l, если она удовлетворяет предикату p, иначе
-- завершается с неуспехом.
satisfy :: (lex -> Bool) -> Parser lex lex
satisfy p =
  Parser $ do
    l:ls' <- get
    if p l
      then put ls' >> pure l
      else lift Nothing

-- eof завершается с успехом если мы достигли конца строки.
eof :: Parser lex ()
eof =
  Parser $ do
    ls <- get
    unless (null ls) (lift Nothing)

instance Functor (Parser lex) where
  fmap = liftM

instance Applicative (Parser lex) where
  pure = return
  (<*>) = ap

instance Monad (Parser lex) where
  return = Parser . pure
  Parser p >>= k = Parser $ p >>= unParser . k

instance Alternative (Parser lex) where
  empty = Parser $ lift Nothing
  p1 <|> p2 =
    Parser $ do
      ls <- get
      case runParser p1 ls of
        Nothing ->
          case runParser p2 ls of
            Nothing       -> lift Nothing
            Just (ls', x) -> put ls' >> pure x
        Just (ls'', x') -> put ls'' >> pure x'
