{-# LANGUAGE OverloadedStrings #-}
import Parser (readExpr, readExprFile)
import LispVal (LispVal(..), Eval(..), Env(..), showVal, LispException(..), Fun(Fun))
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Prim (primEnv, unop)
import Eval (basicEnv, eval, runASTinEnv, fileToEvalForm)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Control.Exception
import Data.Map as M
import Control.Monad.Reader
import Control.Monad.Except (runExcept)
import System.IO (readFile)


main :: IO ()
main = do
    test <- readFile "test.phl"
    let result = readExprFile $ T.pack test
    case result of
        Left err -> print err
        Right val ->  do
            -- Show the AST
            print val
            let env = basicEnv
            let ast = fileToEvalForm $ T.pack test
            runASTinEnv env ast >>= print

            

