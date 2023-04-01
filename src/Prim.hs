{-# LANGUAGE OverloadedStrings #-}

module Prim (primEnv, unop) where

import Val
    ( SchemeException(NumArgs, TypeMismatch, ExpectedList),
      Fn(Fn),
      Scheme(Atom, List, Number, String, Bool, Function, Nil),
      Eval )

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Exception
import Control.Monad.Except ( foldM, MonadIO(liftIO) )

type Prim = [ (T.Text, Scheme) ]
type Unary = Scheme -> Eval Scheme
type Binary = Scheme -> Scheme -> Eval Scheme

mkF :: ([Scheme] -> Eval Scheme) -> Scheme
mkF = Function . Fn

-- primEnv contains all of the primitive functions that are available
primEnv :: Prim
primEnv = [
    ("+"     , mkF $ binopFold (numOp    (+))  (Number 0) )
  , ("*"     , mkF $ binopFold (numOp    (*))  (Number 1) )
  --, ("string-append", mkF $ binopFold (strOp    (<>)) (String "") )
  , ("-"     , mkF $ binop $    numOp    (-))
  , ("<"     , mkF $ binop $    numCmp   (<))
  , ("<="    , mkF $ binop $    numCmp   (<=))
  , (">"     , mkF $ binop $    numCmp   (>))
  , (">="    , mkF $ binop $    numCmp   (>=))
  , ("=="    , mkF $ binop $    numCmp   (==))
  , ("even?" , mkF $ unop $     numBool   even)
  , ("odd?"  , mkF $ unop $     numBool   odd)
  , ("neg?"  , mkF $ unop $     numBool (< 0))
  , ("pos?"  , mkF $ unop $     numBool (> 0))
  --, ("eq?"   , mkF $ binop eqCmd )
  --, ("null?" , mkF $ unop (eqCmd Nil) )
  --, ("bl-eq?", mkF $ binop $ eqOp (==))
  --, ("and"   , mkF $ binopFold (eqOp (&&)) (Bool True))
  --, ("or"    , mkF $ binopFold (eqOp (||)) (Bool False))
  --, ("not"   , mkF $ unop notOp)
  , ("cons"  , mkF Prim.cons)
  , ("cdr"   , mkF Prim.cdr)
  , ("car"   , mkF Prim.car)
  , ("quote" , mkF quote)
  --, ("file?" , mkF $ unop fileExists)
  --, ("slurp" , mkF $ unop slurp)
  --, ("wslurp", mkF $ unop wSlurp)
  --, ("put"   , mkF $ binop put)
  ]
  
unop :: Unary -> [Scheme] -> Eval Scheme
unop f [x] = f x
unop _ args = throw $ NumArgs 1 args

binop :: Binary -> [Scheme] -> Eval Scheme
binop f [x, y] = f x y
binop _ args = throw $ NumArgs 2 args


binopFold :: Binary -> Scheme -> [Scheme] -> Eval Scheme
binopFold op farg args = case args of
                            [a,b]  -> op a b
                            (_a:_as) -> foldM op farg args
                            []-> throw $ NumArgs 2 args

numBool :: (Integer -> Bool) -> Scheme -> Eval Scheme
numBool f (Number n) = return $ Bool $ f n
numBool _ x = throw $ TypeMismatch "number" x

numOp :: (Integer -> Integer -> Integer) -> Scheme -> Scheme -> Eval Scheme
numOp op (Number a) (Number b) = return $ Number $ op a b
numOp _ (Number a) Nil = return $ Number a
numOp _ Nil (Number b) = return $ Number b
numOp _ x (Number b) = throw $ TypeMismatch "number" x
numOp _ (Number a) x = throw $ TypeMismatch "number" x
numOp _ x _ = throw $ TypeMismatch "number" x

numCmp :: (Integer -> Integer -> Bool) -> Scheme -> Scheme -> Eval Scheme
numCmp op (Number a) (Number b) = return $ Bool $ op a b
numCmp _ a (Number _) = throw $ TypeMismatch "boolop" a
numCmp _ (Number _) b = throw $ TypeMismatch "boolop" b
numCmp _ a _ = throw $ TypeMismatch "boolop" a

cons :: [Scheme] -> Eval Scheme
cons [x, List xs] = return $ List $ x:xs
cons [x, y] = return $ List [x, y]
cons _ = throw $ ExpectedList "cons, in second argument" 

car :: [Scheme] -> Eval Scheme
car [List (x:_)] = return x
car [List []] = return Nil
car [] = return Nil
car _ = throw $ ExpectedList "car"

cdr :: [Scheme] -> Eval Scheme
cdr [List (_:xs)] = return $ List xs
cdr [List []] = return Nil
cdr [] = return Nil
cdr _ = throw $ ExpectedList "cdr"

quote :: [Scheme] -> Eval Scheme
quote [List xs] = return $ List $ Atom "quote" : xs
quote [expr] = return $ List [Atom "quote", expr]
quote args = throw $ NumArgs 1 args

