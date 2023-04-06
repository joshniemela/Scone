{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Eval (basicEnv, eval) where

import Control.Exception
import Control.Monad.State
import Data.Map as M
import Data.Text as T
import LispVal (Env (..), Eval (unEval), Fun (Fun), LispException (..), LispVal (..), showVal)
import Parser
import Prim (primEnv, unop)
import Text.Megaparsec

basicEnv :: Env
basicEnv = Env{env = M.fromList primEnv}

getVar :: T.Text -> Eval LispVal
getVar var = do
    e <- get
    case M.lookup var (env e) of
        Just val -> return val
        Nothing -> throw $ UnboundVar var

eval :: LispVal -> Eval LispVal
-- Quote
eval (List [Atom "quote", val]) = return val
eval (Atom "dumpEnv") = do
    e <- get
    let keys = M.keys (env e)
    let vals = M.elems (env e)
    return $ List $ Prelude.zipWith (\k v -> List [Atom k, v]) keys vals

-- Autoquote
eval (Atom a) = getVar a
eval (String s) = return $ String s
eval (Number i) = return $ Number i
eval (Bool b) = return $ Bool b
eval Nil = return Nil
eval (List []) = return Nil
-- `if` statement
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        Bool True -> eval conseq
        _ -> throw $ TypeMismatch "if expects a boolean, got: " result

-- Evaluate one element at a time and sequentially update the environment with the new bindings
eval (List (Atom "list" : xs)) = do
    e <- get
    vals <- mapM eval xs
    put e
    return $ List vals

-- `define` function, this is supposed to define a variable in the current scope and also give it to itself for recursion. Also check if the variable is already defined.
eval (List [Atom "define", Atom var, defExpr]) = do
    e <- get
    val <- eval defExpr
    if M.member var (env e)
        then throw $ AlreadyDefined var
        else put $ Env $ M.insert var val (env e)
    -- Return void
    return Nil

-- Takes a lambda atom, a list of atoms and the body
eval (List [Atom "lambda", List params, expr]) = gets (Closure (Fun $ applyLambda expr params))
  where
    applyLambda expr params args = do
        e <- get
        -- Pair each parameter with the argument
        let vars = zipWithError (\p a -> (extractVar p, a)) params args

        -- Add the new bindings to the environment and evaluate the expression
        put (Env $ M.union (M.fromList vars) (env e))
        eval expr
      where
        -- Check if the number of parameters and arguments match
        zipWithError f xs ys =
            if lenX == lenY
                then Prelude.zipWith f xs ys
                else throw $ NumArgs (toInteger lenX) ys
          where
            lenX = Prelude.length xs
            lenY = Prelude.length ys

-- Function application
eval (List (fn : args)) = do
    e <- get
    funVar <- eval fn
    vals <- mapM eval args
    case funVar of
        (Primitive (Fun f)) -> f vals
        (Closure (Fun f) (Env benv)) -> do
            put $ Env $ env e <> benv
            f vals
        _ -> throw $ NotFunction fn

eval (Markup m) = return $ Markup m

eval x = throw $ Default x

extractVar :: LispVal -> T.Text
extractVar (Atom a) = a
extractVar n = throw $ TypeMismatch "expected an atomic value" n
