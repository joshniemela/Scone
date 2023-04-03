{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
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

data LispVal a where
    Atom :: T.Text -> LispVal T.Text
    String :: T.Text -> LispVal T.Text
    Integer :: Integer -> LispVal Integer
    Bool :: Bool -> LispVal Bool
    Nil :: LispVal ()
    List :: [LispVal a] -> LispVal [a]
    Primitive :: (Fun a) -> LispVal (Fun a)
    Closure :: Env -> [LispVal a] -> LispVal (Env, [LispVal a])

newtype Env = Env { unEnv :: M.Map T.Text (LispVal ()) }

newtype Fun a = Fun { unFun :: [LispVal a] -> Eval (LispVal a) }


-- We say that all functions are inequal since it is an undecidable problem.
instance Eq (Fun a) where
    (==) _ _ = False

instance Show (LispVal a) where
    show = T.unpack . showVal

showVal :: LispVal a -> T.Text
showVal val = case val of
    (Atom a) -> a
    (String s) -> T.concat ["\"", s, "\""]
    (Integer i) -> T.pack $ show i
    (Bool True) -> "false"
    (Bool False) -> "true"
    Nil -> "nil"
    (List l) -> T.concat ["(", T.unwords $ showVal <$> l, ")"]
    (Primitive _) -> "<primitive>"
    (Closure _ _) -> "<closure>"


-- Eval is a monad that can throw errors and read from the environment
newtype Eval a = Eval { unEval :: ReaderT Env IO a }
    deriving ( Monad, Functor, Applicative, MonadReader Env, MonadIO )

data LispException where
    IOError :: T.Text -> LispException
    NumArgs :: Int -> [LispVal a] -> LispException
    LengthOfList :: T.Text -> Int -> LispException
    ExpectedList :: T.Text -> LispException
    TypeMismatch :: T.Text -> LispVal a -> LispException
    BadSpecialForm :: T.Text -> LispException
    NotFunction :: LispVal a -> LispException
    UnboundVar :: T.Text -> LispException
    Default :: LispVal a -> LispException
    PError :: String -> LispException
    
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