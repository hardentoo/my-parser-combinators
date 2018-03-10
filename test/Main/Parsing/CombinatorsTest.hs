module Main.Parsing.CombinatorsTest
  ( main
  ) where

import           Control.Monad            (void)
import           Data.Char                (isDigit)
import           Data.Maybe               (isNothing)
import           Main.Parsing.Combinators
import           Main.Parsing.Parser      (runParser)
import           Test.HUnit

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> ([lex], a) -> Test
parserTestOK p s r = runParser p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail p s = TestCase $ assertBool "Parser should fail" $ isNothing (runParser p s)

boolP :: Parser Char Bool
boolP = True <$ string "True" <|> False <$ string "False"

maybeP :: Parser Char a -> Parser Char (Maybe a)
maybeP p = Just <$> (string "Just " *> p) <|> Nothing <$ string "Nothing"

listP :: Parser Char a -> Parser Char [a]
listP p = brackets $ sepBy p $ lexeme ','

listP' :: Parser Char a -> Parser Char [a]
listP' p = brackets $ sepBy p $ between spaces spaces (lexeme ',')

main :: IO ()
main =
  void $
  runTestTT $
  test $
  label
    "pure"
    [parserTestOK (pure 4) "qwerty" === (4, "qwerty"), parserTestOK (pure 'x') "" === ('x', "")] ++
  label
    "empty"
    [parserTestFail (empty :: Parser Char ()) "qwerty", parserTestFail (empty :: Parser Char ()) ""] ++
  label
    "satisfy"
    [ parserTestOK (satisfy (/= 'x')) "qwerty" === ('q', "werty")
    , parserTestFail (satisfy (/= 'x')) "xwerty"
    ] ++
  label
    "parse"
    [ parse (pure 22) "" ~?= Just 22
    , parse (pure 22) "foo" ~?= Nothing
    , parse (satisfy (== 'x')) "x" ~?= Just 'x'
    , parse (satisfy (== 'x')) "xy" ~?= Nothing
    ] ++
  label
    "<*>"
    [ parserTestOK (max <$> satisfy (== 'q') <*> satisfy (/= 'x')) "qwerty" === ('w', "erty")
    , parserTestOK ((+) <$> digit <*> digit) "5678" === (11, "78")
    , parserTestFail(undefined <$> satisfy (== 'q') <*> satisfy (== 'x') :: Parser Char ()) "qwerty"
    , parserTestFail (undefined <$> satisfy (== 'x') <*> satisfy (== 'w') :: Parser Char ()) "qwerty"
    ] ++
  label
    "<|>"
    [ parserTestOK (satisfy (== 'q') <|> satisfy (== 'x')) "qwerty" === ('q', "werty")
    , parserTestOK (satisfy (== 'x') <|> satisfy (== 'q')) "qwerty" === ('q', "werty")
    , parserTestFail (satisfy (== 'x') <|> satisfy (== 'y')) "qwerty"
    ] ++
  label "eof" [parserTestFail eof "qwerty", parserTestOK eof "" === ((), "")] ++
  label
    "lexeme"
    [parserTestOK (lexeme 'q') "qwerty" === ((), "werty"), parserTestFail (lexeme 'x') "qwerty"] ++
  label
    "anyLexeme"
    [parserTestOK anyLexeme "qwerty" === ('q', "werty"), parserTestFail anyLexeme ""] ++
  label
    "digit"
    [ parserTestFail digit "qwerty"
    , parserTestOK digit "123qwerty" === (1, "23qwerty")
    , parserTestFail digit ""
    ] ++
  label
    "string"
    [ parserTestOK (string "qwerty") "qwerty" === ((), "")
    , parserTestOK (string "qwerty") "qwertyuiop" === ((), "uiop")
    , parserTestFail (string "qwerty") "qwerryuiop"
    , parserTestFail (string "qwerty") "qwert"
    ] ++
  label
    "oneOf"
    [ parserTestFail (oneOf "xyz") "qwerty"
    , parserTestOK (oneOf "xyz") "xwerty" === ('x', "werty")
    , parserTestOK (oneOf "xyz") "ywerty" === ('y', "werty")
    , parserTestOK (oneOf "xyz") "zwerty" === ('z', "werty")
    ] ++
  label
    "many"
    [ parserTestOK (many (lexeme 'q')) "qwerty" === ([()], "werty")
    , parserTestOK (many (lexeme 'q')) "qqqwerty" === ([(), (), ()], "werty")
    , parserTestOK (many (lexeme 'q')) "werty" === ([], "werty")
    , parserTestOK (many (lexeme 'q')) "" === ([], "")
    ] ++
  label
    "many1"
    [ parserTestOK (many1 (lexeme 'q')) "qwerty" === ([()], "werty")
    , parserTestOK (many1 (lexeme 'q')) "qqqwerty" === ([(), (), ()], "werty")
    , parserTestFail (many1 (lexeme 'q')) "werty"
    , parserTestFail (many1 (lexeme 'q')) ""
    ] ++
  label
    "natural"
    [ parserTestFail natural "qwerty"
    , parserTestOK natural "123qwerty" === (123, "qwerty")
    , parserTestFail natural "-123qwerty"
    , parserTestFail natural ""
    ] ++
  label
    "integer"
    [ parserTestFail integer "qwerty"
    , parserTestOK integer "123qwerty" === (123, "qwerty")
    , parserTestOK integer "-123qwerty" === (-123, "qwerty")
    , parserTestFail integer "-qwerty"
    ] ++
  label
    "spaces"
    [ parserTestOK spaces "qwerty" === ((), "qwerty")
    , parserTestOK spaces "    qwerty" === ((), "qwerty")
    , parserTestOK spaces "" === ((), "")
    ] ++
  label
    "try"
    [ parserTestOK (try natural) "123qwerty" === (Just 123, "qwerty")
    , parserTestOK (try natural) "qwerty" === (Nothing, "qwerty")
    , parserTestOK (try (lexeme 'q')) "qwerty" === (Just (), "werty")
    , parserTestOK (try (lexeme 'x')) "qwerty" === (Nothing, "qwerty")
    , parserTestOK (try (lexeme 'x')) "" === (Nothing, "")
    , parserTestOK (try eof) "qwerty" === (Nothing, "qwerty")
    , parserTestOK (try eof) "" === (Just (), "")
    ] ++
  label
    "endBy"
    [ parserTestOK (natural `endBy` lexeme ';') "1;2;3;456;xyz;" === ([1, 2, 3, 456], "xyz;")
    , parserTestOK (natural `endBy` lexeme ';') "1;2;3;456" === ([1, 2, 3], "456")
    , parserTestOK (natural `endBy` spaces) "12 25   300" === ([12, 25, 300], "")
    , parserTestOK (natural `endBy` spaces) "qwerty" === ([], "qwerty")
    , parserTestOK (natural `endBy` spaces) "" === ([], "")
    ] ++
  label
    "endBy1"
    [ parserTestOK (natural `endBy1` lexeme ';') "1;2;3;456;xyz;" === ([1, 2, 3, 456], "xyz;")
    , parserTestOK (natural `endBy1` lexeme ';') "1;2;3;456" === ([1, 2, 3], "456")
    , parserTestOK (natural `endBy1` spaces) "12 25   300" === ([12, 25, 300], "")
    , parserTestFail (natural `endBy1` spaces) "qwerty"
    , parserTestFail (natural `endBy1` spaces) ""
    ] ++
  label
    "sepBy"
    [ parserTestOK (natural `sepBy` lexeme ';') "1;2;3;456;xyz;" === ([1, 2, 3, 456], ";xyz;")
    , parserTestOK (natural `sepBy` lexeme ';') "1;2;3;456" === ([1, 2, 3, 456], "")
    , parserTestOK (natural `sepBy` spaces) "12 25   300" === ([12, 25, 300], "")
    , parserTestOK (natural `sepBy` spaces) "qwerty" === ([], "qwerty")
    , parserTestOK (natural `sepBy` spaces) "" === ([], "")
    ] ++
  label
    "sepBy1"
    [ parserTestOK (natural `sepBy1` lexeme ';') "1;2;3;456;xyz;" === ([1, 2, 3, 456], ";xyz;")
    , parserTestOK (natural `sepBy1` lexeme ';') "1;2;3;456" === ([1, 2, 3, 456], "")
    , parserTestOK (natural `sepBy1` spaces) "12 25   300" === ([12, 25, 300], "")
    , parserTestFail (natural `sepBy1` spaces) "qwerty"
    , parserTestFail (natural `sepBy1` spaces) ""
    ] ++
  label
    "between"
    [ parserTestFail (between (lexeme 'a') (lexeme 'b') (lexeme 'c')) "abc"
    , parserTestOK (between (lexeme 'a') (lexeme 'b') (lexeme 'c')) "acb" === ((), "")
    ] ++
  label
    "brackets"
    [ parserTestOK (brackets (string "qwerty")) "[qwerty]uiop" === ((), "uiop")
    , parserTestFail (brackets (string "qwerty")) "[qwertyu]iop"
    ] ++
  label
    "parens"
    [ parserTestOK (parens spaces) "(   )qwerty" === ((), "qwerty")
    , parserTestFail (parens spaces) "(q)werty"
    ] ++
  label
    "braces"
    [parserTestOK (braces natural) "{123}" === (123, ""), parserTestFail (braces natural) "{}"] ++
  label
    "angles"
    [parserTestOK (angles digit) "<1>" === (1, ""), parserTestFail (angles digit) "<1 >"] ++
  label
    "boolP"
    [ parserTestOK boolP "Trueqwerty" === (True, "qwerty")
    , parserTestOK boolP "False" === (False, "")
    , parserTestFail boolP "qwerty"
    ] ++
  label
    "maybeP"
    [ parserTestOK (maybeP natural) "Nothingqwerty" === (Nothing, "qwerty")
    , parserTestOK (maybeP natural) "Just 123qwerty" === (Just 123, "qwerty")
    , parserTestFail (maybeP natural) "Just123qwerty"
    ] ++
  label
    "listP"
    [ parserTestOK (listP integer) "[1,-23,25,347]" === ([1, -23, 25, 347], "")
    , parserTestFail (listP integer) "[1 ,  -23,  25   ,347]"
    ] ++
  label
    "listP'"
    [ parserTestOK (listP' integer) "[1,-23,25,347]" === ([1, -23, 25, 347], "")
    , parserTestOK (listP' integer) "[1 ,  -23,  25   ,347]" === ([1, -23, 25, 347], "")
    ] ++
  label
    "foldl1P"
    [ parserTestOK
        (foldl1P (\a b c -> a ++ "[" ++ show b ++ "]" ++ c) (many $ satisfy $ not . isDigit) digit)
        "a1bcd5efg853h" ===  ("a[1]bcd[5]efg[8][5][3]h", "")
    , parserTestOK
        (foldl1P (\a b c -> "(" ++ a ++ [b] ++ c ++ ")") (fmap show natural) anyLexeme)
        "12+34+45+56" === ("(((12+34)+45)+56)", "")
    ] ++
  label
    "foldr1P"
    [ parserTestOK
        (foldr1P (\a b c -> a ++ "[" ++ show b ++ "]" ++ c) (many $ satisfy $ not . isDigit) digit)
        "a1bcd5efg853h" === ("a[1]bcd[5]efg[8][5][3]h", "")
    , parserTestOK
        (foldr1P (\a b c -> "(" ++ a ++ [b] ++ c ++ ")") (fmap show natural) anyLexeme)
        "12+34+45+56" === ("(12+(34+(45+56)))", "")
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i, t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1 ..]
    a === (r, s) = a (s, r)
