{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Parser (readExpr, readExprFile)
import LispVal (LispVal(..))
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


-- Read a file and print the result
main :: IO ()
main = do
    contents <- readFile "test.phl"
    case readExprFile (T.pack contents) of
        Left err -> print err
        Right val -> print val