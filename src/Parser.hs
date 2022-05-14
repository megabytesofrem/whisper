{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types (LispVal (..))

-- * Parsec Parser type

type Parser = Parsec Void T.Text

-- * Parsers

-- Spaces consumer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

-- Wrap each lexeme with the spaces consumer sc
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse a symbol
symbol :: Parser LispVal
symbol = do
  let chars = ['+', '-', '*', '/', '%', '<', '>', '=']
  Symbol . T.pack <$> some (alphaNumChar <|> satisfy (`elem` chars))

-- Parse either a decimal number or float
number :: Parser LispVal
number = Number <$> (lexeme L.decimal <|> lexeme L.float)

-- Parse a string
stringLit :: Parser LispVal
stringLit = String . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- Parse either an atom or a S expression
lispVal :: Parser LispVal
lispVal = do
  List
    <$> between (char '(') (char ')') (many lispVal)
    <|> lexeme number
    <|> lexeme symbol
    <|> lexeme stringLit

-- | Run the parser
whispRunParser :: T.Text -> Either (ParseErrorBundle T.Text Void) LispVal
whispRunParser = parse lispVal "<input>"