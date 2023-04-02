{-# LANGUAGE OverloadedStrings #-}
module Prim (primEnv, unop) where

import LispVal
    (LispException(NumArgs, IOError, TypeMismatch, ExpectedList),
    Fun(Fun),
    LispVal(..),
    Eval)

import Data.Text as T
import Data.Map as M
import Control.Exception


type Prim = [(T.Text, LispVal)]
type Unop = LispVal -> Eval LispVal
type Binop = LispVal -> LispVal -> Eval LispVal

mkFun :: ([LispVal] -> Eval LispVal) -> LispVal
mkFun = Primitive . Fun


primEnv :: Prim
primEnv = []

unop :: Unop -> [LispVal] -> Eval LispVal
unop f [x] = f x
unop _ args = throw $ NumArgs 1 args