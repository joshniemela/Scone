{-# LANGUAGE OverloadedStrings #-}

module Prim (primEnv, unop) where

import Control.Exception
import Control.Monad.Reader
import Data.Map as M
import Data.Text as T
import LispVal (
    Env,
    Eval,
    Fun (Fun),
    LispException (ExpectedList, IOError, NumArgs, TypeMismatch),
    LispVal (Atom, Bool, List, Number, Primitive, String),
    showVal,
 )
import Parser

type Prim = [(T.Text, LispVal)]
type Unop = LispVal -> Eval LispVal
type Binop = LispVal -> LispVal -> Eval LispVal

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Primitive . Fun

primEnv :: Prim
primEnv =
    -- Mathematical operators
    [ ("+", mkF $ binopFold (numOp (+)) (Number 0))
    , ("*", mkF $ binopFold (numOp (*)) (Number 1))
    , ("-", mkF $ binop $ numOp (-))
    , ("/", mkF $ binop $ numOp div)
    , ("mod", mkF $ binop $ numOp mod)
    , ("%", mkF $ binop $ numOp mod)
    , ("^", mkF $ binop $ numOp (^))
    , ("quotient", mkF $ binop $ numOp quot)
    , ("remainder", mkF $ binop $ numOp rem)  

    -- Unary predicates
    , ("even?", mkF $ unop $ numBool even)
    , ("odd?", mkF $ unop $ numBool odd)
    , ("neg?", mkF $ unop $ numBool (< 0))
    , ("pos?", mkF $ unop $ numBool (> 0))
    , ("zero?", mkF $ unop $ numBool (== 0))
    , ("list?", mkF $ unop $ \x -> return $ Bool $ case x of List _ -> True; _ -> False)
    , ("string?", mkF $ unop $ \x -> return $ Bool $ case x of String _ -> True; _ -> False)
    , ("atom?", mkF $ unop $ \x -> return $ Bool $ case x of Atom _ -> True; _ -> False)
    , ("number?", mkF $ unop $ \x -> return $ Bool $ case x of Number _ -> True; _ -> False)
    , ("bool?", mkF $ unop $ \x -> return $ Bool $ case x of Bool _ -> True; _ -> False)
    , ("null?", mkF $ unop $ \x -> return $ Bool $ case x of List [] -> True; _ -> False)
    , ("false?", mkF $ unop $ \x -> return $ Bool $ case x of Bool False -> True; _ -> False)
    ]

unop :: Unop -> [LispVal] -> Eval LispVal
unop op [x] = op x
unop _ args = throw $ NumArgs 1 args

binop :: Binop -> [LispVal] -> Eval LispVal
binop op [x, y] = op x y
binop _ args = throw $ NumArgs 2 args

binopFold :: Binop -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
    [a, b] -> op a b
    (_a : _as) -> foldM op farg args
    [] -> throw $ NumArgs 2 args

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool _ x = throw $ TypeMismatch "numeric op " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x y
numOp _ x (Number _) = throw $ TypeMismatch "numeric op " x
numOp _ (Number _) y = throw $ TypeMismatch "numeric op " y
numOp _ x _ = throw $ TypeMismatch "numeric op " x

boolOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
boolOp op (Bool x) (Bool y) = return $ Bool $ op x y
boolOp _ x (Bool _) = throw $ TypeMismatch "boolean op " x
boolOp _ (Bool _) y = throw $ TypeMismatch "boolean op " y
boolOp _ x _ = throw $ TypeMismatch "boolean op " x
