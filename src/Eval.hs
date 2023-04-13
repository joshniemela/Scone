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

dropNil :: [LispVal] -> [LispVal]
dropNil [] = []
dropNil (List []:xs) = dropNil xs
dropNil (x:xs) = x : dropNil xs


-- Evaluate various types of quotes
eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = return val
eval (List [Atom "quasiquote", form]) =
    evalUnquotes form
    where
        evalUnquotes (List [Atom "unquote", form]) = eval form
        evalUnquotes (List (x:xs)) = List <$> mapM evalUnquotes (x:xs)
        evalUnquotes x = return x 
eval (List [Atom "unquote", form]) = throw $ BadSpecialForm "unquote outside of quasiquote"

-- (x y z (markup a b c) foo bar) -> (x y z a b c foo bar)
-- Evaluates all the expressions in the list and then concatenates the results and puts the environment back to the original one, note how this is the same as list but `get` is called after evaluating the expressions.
eval (List (Atom "markup": xs)) = do
    vals <- mapM eval xs
    e' <- get
    put $ Env $ env e'
    
    -- Put spaces between the values and drop the nils
    return $ Markup $ T.intercalate " " $ showVal <$> dropNil vals





-- Autoquote
eval (Atom a) = getVar a
eval (String s) = return $ String s
eval (Number i) = return $ Number i
eval (Bool b) = return $ Bool b
eval (Markup m) = return $ Markup m

-- `if` statement
-- (if pred conseq alt)
eval (List [Atom "if", pred, conseq, alt]) = do
    result <- eval pred
    case result of
        Bool False -> eval alt
        Bool True -> eval conseq
        _ -> throw $ TypeMismatch "if expects a boolean, got: " result

-- `cond` statement, try each predicate until one is true, then evaluate the corresponding expression
-- (cond (pred1 conseq1) (pred2 conseq2) ...)
{-eval (List [Atom "cond" : xs]) = do
    e <- get
    case xs of
        [] -> throw $ BadSpecialForm "cond expects at least one predicate"
        (List [pred, conseq] : rest) -> do
            result <- eval pred
            case result of
                Bool False -> eval $ List [Atom "cond" : rest]
                Bool True -> eval conseq
                _ -> throw $ TypeMismatch "cond expects a boolean, got: " result
        _ -> throw $ BadSpecialForm "cond expects a list of predicates"
-}

-- Evaluate one element at a time and sequentially update the environment with the new bindings
-- (list (+ 1 2) (+ 3 4)) -> (3 7)
eval (List (Atom "list" : xs)) = do
    e <- get
    vals <- mapM eval xs
    put e
    return $ List vals

-- `define` function, this is supposed to define a variable in the current scope and also give it to itself for recursion. Also check if the variable is already defined.
-- (define x 5)
eval (List [Atom "define", Atom var, defExpr]) = do
    e <- get
    val <- eval defExpr
    if M.member var (env e)
        then throw $ AlreadyDefined var
        else put $ Env $ M.insert var val (env e)
    return $ List []

-- Shorthand define
-- (define (f x y) (+ x y))
eval (List [Atom "define", List (Atom var : params), defExpr]) = do
    e <- get
    let fun = List [Atom "lambda", List params, defExpr]
    val <- eval fun
    if M.member var (env e)
        then throw $ AlreadyDefined var
        else put $ Env $ M.insert var val (env e)
    return $ List []

-- Takes a lambda atom, a list of atoms and the body
-- (lambda (x y) (+ x y))
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
-- (f x... )
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


eval x = throw $ Default x

extractVar :: LispVal -> T.Text
extractVar (Atom a) = a
extractVar n = throw $ TypeMismatch "expected an atomic value" n
