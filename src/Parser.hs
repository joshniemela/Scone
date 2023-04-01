{-# LANGUAGE OverloadedStrings #-}

module Parser (
    readExpr,
    readExprFile
) where

import Val (Scheme(..), SchemeException(..))
import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Except

type Parser = Parsec Void Text

parens = between (char '(' <* space) (space <* char ')')

reservedChars :: [Char]
reservedChars = [':', ']', '[', '#', '(', ')', '\'', '"']

escapeChars :: Parser Char
escapeChars = do
    _ <- char '\\'
    oneOf reservedChars

-- These are the characters that are allowed at the start of an atom
allowedFirstChars :: [Char]
allowedFirstChars = ['a'..'z'] ++ ['A'..'Z'] ++ "+-*/$%&<=>?^_~"

allowedFirst :: Parser Char
allowedFirst = oneOf allowedFirstChars


-- Basic types
parseNumber :: Parser Scheme
parseNumber = Number <$> L.decimal

parseAtom :: Parser Scheme
parseAtom = do
    first <- allowedFirst
    rest <- many $ oneOf $ allowedFirstChars ++ ['0'..'9']
    return $ Atom $ T.pack $ first : rest

parseMany :: Parser [Scheme]
parseMany = parseSExpr `sepEndBy` space1

parseList :: Parser Scheme
parseList = List <$> parens parseMany

parseQuoted :: Parser Scheme
parseQuoted = do
    _ <- char '\''
    expr <- parseSExpr
    return $ List [Atom "quote", expr]

parseSExpr :: Parser Scheme
parseSExpr = do
    expr <- choice [parseNumber, parseAtom, parseList, parseQuoted]
    lookAhead $ choice [eof, space]
    return expr

contents :: Parser a -> Parser a
contents p = do
    space
    r <- p
    eof
    return r

readExpr :: Text -> Either (ParseErrorBundle Text Void) Scheme
readExpr = parse (contents parseSExpr) "<stdin>"

readExprFile :: Text -> Either (ParseErrorBundle Text Void) Scheme
readExprFile = parse (contents $ List <$> parseMany) "file"
{-
-- Parsers in markup mode
parseCode :: Parser SExpr
parseCode = char ':' >> parseSExpr
-}

{-
-- TODO: leaves newline and whitespaces at the end
parseMarkup :: Parser SExpr
parseMarkup = do
    space
    contents <- manyTill (escapeChars <|> L.charLiteral) (lookAhead (oneOf reservedChars <|> (eof >> return ' ') ))
    -- Strip last spaces and newlines
    return $ Markup $ T.pack $ reverse $ dropWhile (\x -> x == ' ' || x == '\n') $ reverse contents
-}

{-
-- Parsers in code mode
parseMarkupBlock :: Parser SExpr
parseMarkupBlock = do
    _ <- char '['
    contents <- manyTill (parseCode <|> parseMarkup) (char ']')
    return $ Content contents

parseContent :: Parser SExpr
parseContent = do
    exprs <- manyTill (parseCode <|> parseMarkup) eof
    -- remove empty markup
    return $ Content $ filter (\x -> case x of
        Markup "" -> False
        _ -> True) exprs
-}