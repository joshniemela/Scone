{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad.Except (runExcept)
import Control.Monad.State
import Data.Map as M
import Data.Text as T
import Data.Void
import Eval (basicEnv, eval)
import LispVal (Env (..), Eval (..), Fun (Fun), LispException (..), LispVal (..), showVal)
import Parser (readExpr, readExprs)
import Prim (primEnv, unop)
import System.IO (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runASTinEnv :: Env -> Eval b -> IO b
runASTinEnv env ast = evalStateT (unEval ast) env

main :: IO ()
main = do
    test <- readFile "test.phl"
    let result = readExprs $ T.pack test
    case result of
        Left err -> putStrLn $ errorBundlePretty err
        Right val -> do
            -- Eval
            let env = basicEnv
            putStrLn "Raw:"
            -- Prettyprint
            putStrLn $ T.unpack $ showVal val

            putStrLn "Eval:"
            let ast = eval val
            runASTinEnv env ast >>= putStrLn . T.unpack . showVal
