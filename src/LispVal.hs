{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal (
    LispVal(..),
    Fun(..),
    Eval(..),
    Env(..),
    showVal,
    LispException(..)
) where

import Data.Text as T
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader
import Control.Exception

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

data LispException
  = NumArgs Integer [LispVal]
  | LengthOfList T.Text Int
  | ExpectedList T.Text
  | TypeMismatch T.Text LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String -- from show anyway
  | IOError T.Text

instance Exception LispException

instance Show LispException where
    show = T.unpack . showError


showError :: LispException -> T.Text
showError err =
  case err of
    (IOError txt)            -> T.concat ["Error reading file: ", txt]
    (NumArgs int args)       -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
    (LengthOfList txt int)   -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
    (ExpectedList txt)       -> T.concat ["Error Expected List in funciton ", txt]
    (TypeMismatch txt val)   -> T.concat ["Error Type Mismatch: ", txt, showVal val]
    (BadSpecialForm txt)     -> T.concat ["Error Bad Special Form: ", txt]
    (NotFunction val)        -> T.concat ["Error Not a Function: ", showVal val]
    (UnboundVar txt)         -> T.concat ["Error Unbound Variable: ", txt]
    (PError str)             -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
    (Default val)            -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]

unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $  showVal <$> list
