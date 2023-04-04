{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LispVal (
  LispVal(..),
  Eval(..),
  Fun(..),
  Env(..),
  LispException(..),
  showVal,
) where

import Data.Text as T
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader
import Control.Exception

data LispVal =
    Atom T.Text
    | String T.Text
    | Number Integer
    | Bool Bool
    | Nil
    | List [LispVal]
    | Primitive Fun
    | Closure Fun Env

newtype Fun = Fun { fn :: [LispVal] -> Eval LispVal }
newtype Env = Env {env :: M.Map T.Text LispVal}




-- We say that all functions are inequal since it is an undecidable problem.
instance Eq Fun where
    (==) _ _ = False

instance Show LispVal where
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val = case val of
    (Atom a) -> a
    (String s) -> T.concat ["\"", s, "\""]
    (Number i) -> T.pack $ show i
    (Bool True) -> "false"
    (Bool False) -> "true"
    Nil -> "nil"
    (List l) -> T.concat ["(", T.unwords $ showVal <$> l, ")"]
    (Primitive _) -> "<primitive>"
    (Closure _ _) -> "<closure>"


-- Eval is a monad that can throw errors and read from the environment
newtype Eval a = Eval { unEval :: ReaderT Env IO a }
    deriving ( Monad, Functor, Applicative, MonadReader Env, MonadIO )

data LispException =
    IOError T.Text
    | NumArgs Integer [LispVal]
    | LengthOfList T.Text Integer
    | ExpectedList T.Text
    | TypeMismatch T.Text LispVal
    | BadSpecialForm T.Text
    | NotFunction LispVal
    | UnboundVar T.Text
    | PError String
    | Default LispVal

instance Exception LispException

instance Show LispException where
    show = T.unpack . showError


showError :: LispException -> T.Text
showError err = case err of
    (IOError e) -> T.concat ["IO error: ", e]
    (NumArgs expected found) -> T.concat ["Expected ", T.pack $ show expected, " args; found values ", T.unwords $ showVal <$> found]
    (LengthOfList expected found) -> T.concat ["Expected ", expected, " args; found values ", T.pack $ show found]
    (ExpectedList found) -> T.concat ["Expected a list; found ", found]
    (TypeMismatch expected found) -> T.concat ["Invalid type: expected ", expected, ", found ", showVal found]
    (BadSpecialForm message) -> T.concat ["Bad special form: ", message]
    (NotFunction found) -> T.concat ["Not a function: ", showVal found]
    (UnboundVar message) -> T.concat ["Unbound variable: ", message]
    (PError message) -> T.pack message
    (Default found) -> T.concat ["Error: ", showVal found]


