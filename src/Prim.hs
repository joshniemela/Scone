{-# LANGUAGE OverloadedStrings #-}

module Prim (primEnv) where

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

car :: [LispVal] -> Eval LispVal
car [List (x : _)] = return x

cdr :: [LispVal] -> Eval LispVal
cdr [List (_ : xs)] = return $ List xs

consLisp :: [LispVal] -> Eval LispVal
consLisp [x, List xs] = return $ List (x : xs)

cadr :: [LispVal] -> Eval LispVal
cadr [List (_ : x : _)] = return x

add :: [LispVal] -> Eval LispVal
add xs = return $ Number (sum $ (\(Number x) -> x) <$> xs)

prod :: [LispVal] -> Eval LispVal
prod xs = return $ Number (product $ (\(Number x) -> x) <$> xs)

sub :: [LispVal] -> Eval LispVal
sub [Number x, Number y] = return $ Number (x - y)

zero :: [LispVal] -> Eval LispVal
zero [Number x] = return $ Bool (x == 0)

nullLisp :: [LispVal] -> Eval LispVal
nullLisp [List []] = return $ Bool True
nullLisp _ = return $ Bool False

primEnv :: Prim
primEnv =
    [ ("+", mkF add),
      ("*", mkF prod),
      ("-", mkF sub),
      ("car", mkF car),
      ("cdr", mkF cdr),
      ("cons", mkF consLisp),
      ("cadr", mkF cadr),
      ("zero?", mkF zero),
      ("null?", mkF nullLisp)
    ]