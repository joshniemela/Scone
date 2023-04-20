{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal (
    LispVal (..),
    Eval (..),
    Fun (..),
    Env (..),
    LispException (..),
    showVal,
) where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as M
import Data.Text as T

data LispVal
    = Atom T.Text
    | Markup T.Text
    | String T.Text
    | Number Integer
    | Bool Bool
    | List [LispVal]
    | Primitive Fun
    | Closure Fun Env
    | Macro Fun Env

instance Eq LispVal where
    (==) (Atom a) (Atom b) = a == b
    (==) (String a) (String b) = a == b
    (==) (Number a) (Number b) = a == b
    (==) (Bool a) (Bool b) = a == b
    (==) (List a) (List b) = a == b
    (==) _ _ = False

newtype Fun = Fun {fn :: [LispVal] -> Eval LispVal}
newtype Env = Env {env :: M.Map T.Text LispVal}

instance Show LispVal where
    -- Show value and their showval
    show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val = case val of
    (Atom a) -> a
    (String s) -> T.concat ["\"", s, "\""]
    (Number i) -> T.pack $ show i
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    (List l) -> T.concat ["(", T.unwords $ showVal <$> l, ")"]
    (Primitive _) -> "<primitive>"
    (Closure _ _) -> "<closure>"
    (Markup m) -> T.concat ["[", m, "]"]

-- Eval is a monad that can throw errors and read from the environment
newtype Eval a = StateEval {unEval :: StateT Env IO a}
    deriving (Monad, Functor, Applicative, MonadState Env, MonadIO)

data LispException
    = IOError T.Text
    | NumArgs Integer [LispVal]
    | LengthOfList T.Text Integer
    | ExpectedList T.Text
    | TypeMismatch T.Text LispVal
    | BadSpecialForm T.Text
    | NotFunction LispVal
    | UnboundVar T.Text
    | PError T.Text
    | AlreadyDefined T.Text
    | Default LispVal

instance Exception LispException

instance Show LispException where
    show = T.unpack . showError

showError :: LispException -> T.Text
showError err = case err of
    (IOError e) -> T.concat ["IO error: ", e]
    (NumArgs expected found) -> case expected of
        1 -> T.concat ["Expected 1 arg; found values ", T.pack $ show found]
        _ -> T.concat ["Expected ", T.pack $ show expected, " args; found values: ", T.pack $ show found]
    (LengthOfList expected found) -> T.concat ["Expected ", expected, " args; found values ", T.pack $ show found]
    (ExpectedList found) -> T.concat ["Expected a list; found ", found]
    (TypeMismatch expected found) -> T.concat ["Invalid type: expected ", expected, ", found ", showVal found]
    (BadSpecialForm message) -> T.concat ["Bad special form: ", message]
    (NotFunction found) -> T.concat ["Not a function: ", showVal found]
    (UnboundVar message) -> T.concat ["Unbound variable: ", message]
    (PError message) -> T.concat ["Parse error: ", message]
    (AlreadyDefined message) -> T.concat ["Multiple definitions for: ", message]
    (Default found) -> T.concat ["Error: ", showVal found]
