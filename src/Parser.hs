{-# LANGUAGE OverloadedStrings #-}

module Parser (
    readExpr,
    readExprFile
) where

import LispVal
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Void

type Parser = Parsec Void T.Text

reservedChars :: [Char]
reservedChars = "()[]\"\';:#"

escapeChars :: Parser Char
escapeChars = do
    _ <- char '\\'
    oneOf reservedChars

parseNumber :: Parser LispVal
parseNumber = do
    sign <- optional $ char '-'
    num <- L.decimal
    return $ Integer $ case sign of
        Just _ -> negate num
        Nothing -> num

firstAllowed :: Parser Char
firstAllowed = letterChar <|> oneOf others
    where others = "!$%&|*+-/:<=>?@^_~" :: [Char]

parseAtom :: Parser LispVal
parseAtom = do
    first <- firstAllowed
    rest <- many $ firstAllowed <|> digitChar
    let atom = first:rest
    return $ case atom of
        "true" -> Bool True
        "false" -> Bool False
        "nil" -> Nil
        _ -> Atom $ T.pack atom

parseMany :: Parser [LispVal]
parseMany = parseSExpr `sepEndBy` space1

parens = between (char '(' <* space) (space <* char ')')

parseList :: Parser LispVal
parseList = List <$> parens parseMany

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    str <- many $ escapeChars <|> noneOf reservedChars
    _ <- char '"'
    return $ String $ T.pack str

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    expr <- parseSExpr
    return $ List [Atom "quote", expr]


parseSExpr :: Parser LispVal
parseSExpr = choice [ parseQuoted, parseList, parseAtom, parseNumber, parseString ]

contents :: Parser a -> Parser a
contents p = do
    space
    r <- p
    eof
    return r



readExpr :: T.Text -> Either (ParseErrorBundle T.Text Void) LispVal
readExpr = parse (contents parseSExpr) "<stdin>"

readExprFile :: T.Text -> Either (ParseErrorBundle T.Text Void) LispVal
readExprFile = parse (contents $ List <$> parseMany) "<file>"