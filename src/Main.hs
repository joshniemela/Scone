{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Void
import Data.Text (Text, dropWhileEnd)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

-- Define all types for the language
data SExpr = List [SExpr]
           | Atom Text
           | Number Int
           | String Text
           | Bool Bool
           | Markup Text
           | Comment Text
           deriving (Show, Eq)

type Parser = Parsec Void Text

parens = between (char '(') (char ')')

-- These are the characters that are allowed at the start of an atom
allowedFirstChars :: [Char]
allowedFirstChars = ['a'..'z'] ++ ['A'..'Z'] ++ "+-*/$%&<=>?^_~"

allowedFirst :: Parser Char
allowedFirst = oneOf allowedFirstChars


-- Basic types
parseNumber :: Parser SExpr
parseNumber = Number <$> L.decimal

parseAtom :: Parser SExpr
parseAtom = do
    first <- allowedFirst
    rest <- many $ oneOf $ allowedFirstChars ++ ['0'..'9']
    return $ Atom $ T.pack $ first : rest

parseList :: Parser SExpr
parseList = List <$> parens (parseSExpr `sepBy` space1)

parseSExpr :: Parser SExpr
parseSExpr = do
    expr <- choice [parseNumber, parseAtom, parseList, parseMarkupBlock]
    lookAhead $ choice [space, eof]
    -- If the code is list of length 1  then return the first element
    case expr of
        List [x] -> return x
        List [] -> return $ Atom "nil"
        _ -> return expr

-- Parsers in markup mode
parseCode :: Parser SExpr
parseCode = char ':' >> parseSExpr

reservedChars :: [Char]
reservedChars = [':', ']', '[', '#', '(', ')', '\'']

escapeChars :: Parser Char
escapeChars = do
    char '\\'
    c <- oneOf reservedChars
    return c

-- TODO: leaves newline and whitespaces at the end
parseMarkup :: Parser SExpr
parseMarkup = do
    contents <- manyTill (escapeChars <|> L.charLiteral) (lookAhead (oneOf reservedChars <|> (eof >> return ' ') ))
    return $ Markup $ T.pack contents


-- Parsers in code mode
parseMarkupBlock :: Parser SExpr
parseMarkupBlock = do
    char '['
    contents <- manyTill (parseCode <|> parseMarkup) (char ']')
    return $ List contents

parseContent :: Parser SExpr
parseContent = do
    space
    exprs <- manyTill (parseCode <|> parseMarkup) eof
    return $ List exprs


-- Test random string
main :: IO ()
main = do
    content <- readFile "test.txt"
    case parse parseContent "test.txt" (T.pack content) of
        Left err -> putStrLn $ errorBundlePretty err
        Right expr -> print expr