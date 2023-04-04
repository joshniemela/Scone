{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Eval (basicEnv, eval, runASTinEnv, fileToEvalForm) where

import LispVal (LispVal(..), Eval(unEval), Env(..), showVal, LispException(..), Fun(Fun))
import Parser
import Data.Text as T
import Data.Map as M
import Control.Exception
import Control.Monad.Reader
import Prim (primEnv, unop)
basicEnv :: Env
basicEnv = Env { env = M.fromList primEnv }

readFn :: LispVal -> Eval LispVal
readFn (String s) = lineToEvalForm s
readFn val = throw $ TypeMismatch "read expects a string, got: " val


lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show  )  eval $ readExpr input

runASTinEnv :: Env -> Eval b -> IO b
runASTinEnv env ast = runReaderT (unEval ast) env

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show  )  evalBody $ readExprFile input


evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  ctx <- ask
  local (const $ updateEnv var evalVal ctx) $ eval rest

evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  ctx <- ask
  local (const $ updateEnv var evalVal ctx) $ evalBody $ List rest

evalBody x = eval x


updateEnv :: T.Text -> LispVal -> Env -> Env
updateEnv var e Env{..} =  Env $ M.insert var e env



getVar :: T.Text -> Eval LispVal
getVar var = do
    Env{..} <- ask
    case M.lookup var env of
        Just val -> return val
        Nothing -> throw $ UnboundVar var

eval :: LispVal -> Eval LispVal
-- Quote
eval (List [Atom "quote", val]) = return val
-- Autoquote
eval (Atom a) = getVar a
eval (String s) = return $ String s
eval (Number i) = return $ Number i
eval (Bool b) = return $ Bool b
eval Nil = return Nil
eval (List []) = return Nil

-- If statement
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        Bool True -> eval conseq
        _ -> throw $ TypeMismatch "if expects a boolean, got: " result

eval (List [Atom "define", varExpr, defExpr]) = do
    Env{} <- ask
    _evalVal <- eval defExpr

    bindArgsEval [varExpr] [defExpr] varExpr

eval (List [Atom "lambda", List params, expr]) = do
    -- Add itself to the environment so it can be recursive
    Env env <- ask
    return $ Closure (Fun $ applyLambda expr params) (Env env)


eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest )) = evalBody $ List rest

eval (List [Atom "dumpEnv", rest]) = do
    Env{..} <- ask
    return $ String $ T.pack $ show env

-- Function application
eval (List (fn : args)) = do
    Env{..} <- ask
    funVar <- eval fn
    vals <- mapM eval args
    case funVar of
        (Primitive (Fun f)) -> f vals
        (Closure (Fun f) (Env env)) -> local (const $ Env env) $ f vals
        _ -> throw $ NotFunction fn


eval x = throw $ Default x




bindArgsEval :: [LispVal] -> [LispVal] -> LispVal -> Eval LispVal
bindArgsEval params args expr = do
    -- Temporarily insert the variable into the environment so it can be recursive

    Env{..} <- ask
    let newVars = Prelude.zip (Prelude.map extractVar params) args
    let newEnv = Env $ M.union (M.fromList newVars) env
    local (const newEnv) $ eval expr



applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = bindArgsEval params args expr


extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
extractVar n = throw $ TypeMismatch "expected an atomic value" n