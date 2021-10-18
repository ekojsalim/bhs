{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import Control.Applicative (liftA2)
import Data.Char
import Data.Functor
import qualified Data.Text as T
import LispVal
import Text.Printf
import Prelude hiding (any)

data ParseError = ParseError
  { errExpected :: String,
    errFound :: String
  }

newtype Parser a = Parser {runParser :: String -> (String, Either ParseError a)}
  deriving (Functor)

instance Show ParseError where
  show (ParseError e f) = printf "expected %s but found %s" e f

instance Applicative Parser where
  pure c = Parser (,Right c)
  pf <*> pa = Parser $ \s -> case runParser pf s of
    (s', Right f) -> fmap f <$> runParser pa s'
    (s', Left e) -> (s', Left e)

instance Monad Parser where
  pa >>= f = Parser $ \s -> case runParser pa s of
    (s', Right a) -> runParser (f a) s'
    (s', Left e) -> (s', Left e)

any :: Parser Char
any = Parser $ \case
  [] -> ("", Left $ ParseError "any character" "the end of the input")
  (x : xs) -> (xs, Right x)

eof :: Parser ()
eof = Parser $ \case
  [] -> ("", Right ())
  s@(c : _) -> (s, Left $ ParseError "the end of the input" [c])

parseError :: String -> String -> Parser a
parseError expected found = Parser (,Left $ ParseError expected found)

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy description predicate = try $ do
  c <- any
  if predicate c
    then pure c
    else parseError description [c]

-- backtracking

try :: Parser a -> Parser a
try p = Parser $ \s -> case runParser p s of
  (_s', Left err) -> (s, Left err)
  success -> success

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  (s', Left err)
    | s' == s -> runParser p2 s
    | otherwise -> (s', Left err)
  success -> success
  
-- repetition

many, many1 :: Parser a -> Parser [a]
many p = many1 p <|> pure []
many1 p = liftA2 (:) p $ many p

sepBy, sepBy1 :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy1 p s <|> pure []
sepBy1 p s = liftA2 (:) p $ many (s >> p)

oneOf s = satisfy ("OneOf" ++ show s) (`elem` s)

noneOf s = satisfy ("NoneOf" ++ show s) (`notElem` s)

-- characters

char c = satisfy [c] (== c)

space = satisfy "space" isSpace

digit = satisfy "digit" isDigit

-- syntax

string = traverse char

token :: Parser a -> Parser a
token p = do a <- p; spaces; return a

spaces = many space

symbol s = string s <* spaces

between o c p = o *> p <* c

brackets = between (symbol "[") (symbol "]")

braces = between (symbol "{") (symbol "}")

parens = between (symbol "(") (symbol ")")

-- Language

reservedNames = []

identifier =
  try $ do
    name <- ident
    if name `elem` reservedNames
      then parseError "reserved word" name
      else return name

iscs = "-+/*=|&><"

identStart = satisfy "IdentifierStart" (\c -> elem c iscs || isLetter c)

ilcs = "?+=|&-/"

identLetter = satisfy "IdentifierLetter" (\c -> isDigit c || isLetter c || elem c ilcs)

ident = do
  c <- identStart
  cs <- many identLetter
  return (c : cs)

reservedOp s = try $ token (string s)

-- Parse LispVal

parseAtom :: Parser LispVal
parseAtom = do
  Atom . T.pack <$> identifier

parseText :: Parser LispVal
parseText = do
  reservedOp "\""
  p <- many1 $ noneOf "\""
  reservedOp "\""
  return $ String . T.pack $ p

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseNegNum :: Parser LispVal
parseNegNum = do
  char '-'
  d <- many1 digit
  return $ Number . negate . read $ d

parseList :: Parser LispVal
parseList =
  List . concat <$> many parseExpr
    `sepBy` (char ' ' <|> char '\n')

parseSExp =
  List . concat
    <$> parens
      ( many parseExpr
          `sepBy` (char ' ' <|> char '\n')
      )

parseQuote :: Parser LispVal
parseQuote = do
  reservedOp "\'"
  x <- parseExpr
  return $ List [Atom $ T.pack "quote", x]

parseReserved :: Parser LispVal
parseReserved =
  do
    reservedOp "Kosong" >> return Nil
    <|> (reservedOp "#b" >> return (Bool True))
    <|> (reservedOp "#s" >> return (Bool False))

parseExpr :: Parser LispVal
parseExpr =
  parseReserved <|> parseNumber
    <|> try parseNegNum
    <|> parseAtom
    <|> parseText
    <|> parseQuote
    <|> parseSExp

run :: Parser a -> String -> Either ParseError a
run p s = snd $ runParser (p <* eof) s

readExpr :: T.Text -> Either ParseError LispVal
readExpr s = run parseExpr $ T.unpack s