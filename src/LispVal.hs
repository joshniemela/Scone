{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal (
    LispVal(..),
    Fun(..),
    Eval,
    Env,
    showVal
) where

import Data.Text as T
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader

data LispVal
    = Atom T.Text
    | List [LispVal]
    | Integer Integer
    | String T.Text
    | Primitive Fun
    | Closure Fun Env
    | Nil
    | Bool Bool deriving (Eq)

newtype Fun = Fun { fn :: [LispVal] -> Eval LispVal} 

-- We say that all functions are inequal since it is an undecidable problem.
instance Eq Fun where
    (==) _ _ = False

instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val = case val of
    (Atom a) -> a
    (String s) -> T.concat ["\"", s, "\""]
    (Integer i) -> T.pack $ show i
    (Bool True) -> "true"
    (Bool False) -> "false"
    Nil -> "nil"
    (List l) -> T.concat ["(", T.unwords $ showVal <$> l, ")"]
    (Primitive _) -> "<primitive>"
    (Closure _ _) -> "<closure>"


-- Environment is defined as a map from Text to LispVal
type Env = M.Map T.Text LispVal

-- Eval is a monad that can throw errors and read from the environment
newtype Eval a = Eval { unEval :: ReaderT Env IO a }
    deriving ( Monad, Functor, Applicative, MonadReader Env, MonadIO )




