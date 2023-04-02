{-# LANGUAGE OverloadedStrings #-}

module Eval (basicEnv, evalFile, runParseTest, runASTinEnv, eval) where

import LispVal (LispVal(..), Fun(..), Eval(unEval), Env, showVal, LispException(..))    
import Parser
import Data.Text as T
import Data.Map as M
import Control.Exception
import Control.Monad.Reader
import Prim (primEnv, unop)
basicEnv :: Env
basicEnv = M.fromList $ primEnv
        <> [("read", Primitive $ Fun $ unop $ readFn)]

readFn :: LispVal -> Eval LispVal
readFn (String s) = lineToEvalForm s
readFn val = throw $ TypeMismatch "read expects a string, got: " val


evalFile :: T.Text -> IO ()
evalFile fileExpr = runASTinEnv basicEnv (fileToEvalForm fileExpr) >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show) 
    evalBody $ readExprFile input

runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

runASTinEnv :: Env -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show) 
    eval $ readExpr input

eval :: LispVal -> Eval LispVal
-- Quote
eval (List [Atom "quote", val]) = return val
-- Autoquote
eval (Integer i) = return $ Integer i
eval (String s) = return $ String s
eval (Bool b) = return $ Bool b
eval (List []) = return Nil
eval Nil = return Nil

-- Write
eval (List [Atom "write", rest]) =
    return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) =
    return . String . T.pack $ show rest

-- Atom
eval n@(Atom _) = getVar n

-- If statements
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        (Bool True) -> eval conseq
        (Bool False) -> eval alt
        _ -> throw $ BadSpecialForm "if"


-- Let statements in the form of (let ((var val) (var val)) expr)
eval (List [Atom "let", pairs, expr]) = do
    env <- ask
    let env' = const $ M.union (extractPairs pairs) env
    local env' $ eval expr
    where extractPairs (List []) = M.empty
          extractPairs (List ((List [Atom var, val]):rest)) = 
            M.insert var val $ extractPairs $ List rest
          extractPairs _ = M.empty

-- Begin
eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest)) = evalBody $ List rest

-- Define
eval (List [Atom "define", var, expr]) = do
    varAtom <- ensureAtom var
    evalVal <- eval expr
    env <- ask
    let env' = const $ M.insert (extractVar varAtom) evalVal env 
        in local env' $ return evalVal

-- Lambda
eval (List [Atom "lambda", List params, expr]) = do
    env <- ask
    return $ Closure (Fun $ applyClosure expr params) env
eval (List (Atom "lambda":_)) = throw $ BadSpecialForm "lambda"

eval (List ((:) x xs)) = do
    fn <- eval x
    xVal <- mapM eval xs
    case fn of
        (Primitive (Fun f)) -> f xVal
        (Closure (Fun f) env) -> local (const env) $ f xVal
        _ -> throw $ NotFunction fn

getVar :: LispVal -> Eval LispVal
getVar (Atom name) = do
    env <- ask
    case M.lookup name env of
        Just val -> return val
        Nothing -> throw $ UnboundVar name


-- Function application
applyClosure :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyClosure expr params args = do
    env <- ask
    argEval <- mapM eval args
    let env' = M.fromList $ Prelude.zip (extractVar <$> params) argEval
        in local (const env') $ evalBody expr

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
    evalVal <- eval defExpr
    env <- ask
    local (const $ M.insert var evalVal env) $ evalBody rest

evalBody (List ((:) (Atom "define") [Atom var, defExpr])) = do
    evalVal <- eval defExpr
    env <- ask
    let envFn = const $ M.insert var evalVal env
        in local envFn $ return evalVal
evalBody x = eval x

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom a) = a