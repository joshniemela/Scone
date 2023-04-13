{-# LANGUAGE OverloadedStrings #-}

module Parser (
    readExpr,
    readExprs,
) where

import qualified Data.Text as T
import Data.Void
import LispVal
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

reservedChars :: [Char]
reservedChars = "()[]\"\';:"

escapeChars :: Parser Char
escapeChars = do
    _ <- char '\\'
    oneOf reservedChars

parseComment :: Parser ()
parseComment = L.skipLineComment ";" *> space


parseNumber :: Parser LispVal
parseNumber = do
    sign <- optional $ char '-'
    num <- L.decimal
    return $
        Number $ case sign of
            Just _ -> negate num
            Nothing -> num

firstAllowed :: Parser Char
firstAllowed = letterChar <|> oneOf others
  where
    others = "#!$%&|*+-/:<=>?@^_~" :: [Char]

parseAtom :: Parser LispVal
parseAtom = do
    first <- firstAllowed
    rest <- many $ firstAllowed <|> digitChar
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom $ T.pack atom

parseMany :: Parser LispVal -> Parser [LispVal]
parseMany x = x `sepEndBy` space1

parens = between (char '(' <* space) (space <* char ')')

parseList :: Parser LispVal
parseList = List <$> parens (parseMany parseSExpr)

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    str <- many $ escapeChars <|> noneOf reservedChars
    _ <- char '"'
    return $ String $ T.pack str

parseBlock :: Parser LispVal
parseBlock = do
    _ <- char '['
    exprs <- many (parseText <* space)
    _ <- char ']'
    return $ List $ Atom "markup" : exprs

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    expr <- parseSExpr
    return $ List [Atom "quote", expr]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    _ <- char '`'
    expr <- parseSExpr
    return $ List [Atom "quasiquote", expr]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    _ <- char ','
    expr <- parseSExpr
    return $ List [Atom "unquote", expr]


parseSExpr :: Parser LispVal
parseSExpr = choice [
  parseComment *> parseSExpr
  , parseQuoted
  , parseQuasiQuoted
  , parseUnQuote
  , parseBlock
  , parseList
  , parseString
  , parseAtom
  , parseNumber
  ]

-- contents is a parser that will eat leading whitespaces and the final EOF.
contents :: Parser a -> Parser a
contents p = do
    space
    r <- p
    eof
    return r

parseCode :: Parser LispVal
parseCode = do
    _ <- char ':'
    parseSExpr

parseMarkup :: Parser LispVal
parseMarkup = do
    -- Take any character that is not ] or :
    markup <- notFollowedBy (char ']' <|> char ':') *> manyTill anySingle (lookAhead $ char ']' <|> char ':')

    --remove trailing whitespaces
    let markup' = reverse $ dropWhile (== ' ') $ reverse markup
    return $ String $ T.pack markup'


parseText :: Parser LispVal
parseText = choice [parseCode, parseMarkup]


readExpr :: T.Text -> Either (ParseErrorBundle T.Text Void) LispVal
readExpr = parse (contents parseSExpr) "expr"

readExprs :: T.Text -> Either (ParseErrorBundle T.Text Void) LispVal
readExprs =
    parse
        ( contents
            ( parseMany parseSExpr >>= \res ->
                -- Concat res to a list prepended with Atom "list"
                return $ List $ Atom "list" : res
            )
        )
        "exprs"
