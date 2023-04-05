{-# LANGUAGE OverloadedStrings #-}
module Prim (primEnv, unop) where

import LispVal
    ( LispException(NumArgs, IOError, TypeMismatch, ExpectedList),
      LispVal(Atom, Primitive, Number, String, Bool, Nil, List),
      Eval, Env, Fun(Fun), showVal )
import Parser
import Data.Text as T
import Data.Map as M
import Control.Exception
import Control.Monad.Reader
import Data.Text as T
import Data.Map as M
import Control.Exception


type Prim = [(T.Text, LispVal)]
type Unop = LispVal -> Eval LispVal
type Binop = LispVal -> LispVal -> Eval LispVal

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Primitive . Fun



primEnv :: Prim
primEnv = [
    ("+"     , mkF $ binopFold (numOp    (+))  (Number 0) )
  , ("*"     , mkF $ binopFold (numOp    (*))  (Number 1) )
  , ("-"     , mkF $ binop $    numOp    (-))
  , ("even?" , mkF $ unop $     numBool   even)
  , ("odd?"  , mkF $ unop $     numBool   odd)
  , ("neg?"  , mkF $ unop $     numBool (< 0))
  , ("pos?"  , mkF $ unop $     numBool (> 0))
  , ("zero?" , mkF $ unop $     numBool (== 0))
  ]

unop :: Unop -> [LispVal] -> Eval LispVal
unop op [x]    = op x
unop _ args    = throw $ NumArgs 1 args

binop :: Binop -> [LispVal] -> Eval LispVal
binop op [x,y]  = op x y
binop _  args   = throw $ NumArgs 2 args

binopFold :: Binop -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
                            [a,b]  -> op a b
                            (_a:_as) -> foldM op farg args
                            []-> throw $ NumArgs 2 args

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool _   x         = throw $ TypeMismatch "numeric op " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp _  Nil        (Number y) = return $ Number y
numOp _  (Number x) Nil        = return $ Number x
numOp _  x          (Number _) = throw $ TypeMismatch "numeric op " x
numOp _  (Number _)  y         = throw $ TypeMismatch "numeric op " y
numOp _  x           _         = throw $ TypeMismatch "numeric op " x