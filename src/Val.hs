{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Val (
    Scheme(..),
    Eval(..),
    Fn(..),
    Env,
    SchemeException(..),
    showScheme,
) where

import qualified Data.Map as M
import Data.Text as T
import Control.Exception
import Control.Monad.Reader (ReaderT(ReaderT), MonadReader, MonadIO)
import Data.Void

data Scheme
    = Atom T.Text
    | List [Scheme]
    | Number Integer
    | String T.Text
    | Bool Bool
    | Function Fn
    | Lambda Fn Env
    | Nil

instance Show Scheme where
    show = T.unpack . showScheme

showScheme :: Scheme -> T.Text
showScheme val = case val of
    (Atom a) -> a
    (String s) -> T.concat ["\"", s, "\""]
    (Number n) -> T.pack $ show n
    (Bool True) -> "true"
    (Bool False) -> "false"
    (List l) -> T.concat ["(", unwordsList l, ")"]
    (Function _) -> "<function>"
    (Lambda _ _) -> "<lambda>"

unwordsList :: [Scheme] -> T.Text
unwordsList list = T.unwords $ showScheme <$> list


data SchemeException
    = NumArgs Integer [Scheme]
    | TypeMismatch T.Text Scheme
    | BadSpecialForm T.Text
    | ExpectedList Scheme
    | NotFunction Scheme
    | UnboundVar T.Text
    | Default Scheme
    
instance Exception SchemeException

instance Show SchemeException where
    show = T.unpack . showExcept

showExcept :: SchemeException -> T.Text
showExcept err =
    case err of
        (NumArgs int args) -> T.concat ["Expected ", T.pack $ show int, " args; found values ", T.unwords $ showScheme <$> args]

        (TypeMismatch expected found) -> T.concat ["Invalid type: expected ", expected, ", found ", showScheme found]

        (BadSpecialForm msg) -> T.concat ["Bad special form: ", msg]

        (ExpectedList found) -> T.concat ["Expected a list, found ", showScheme found]

        (NotFunction found) -> T.concat ["Expected a function, found ", showScheme found]

        (UnboundVar msg) -> T.concat ["Unbound variable: ", msg]

        (Default val) -> T.concat ["Error: ", showScheme val]

    




newtype Fn = Fn { fn :: [Scheme] -> Eval Scheme }

type ValLookup = M.Map T.Text Scheme
type FnLookup = M.Map T.Text Fn

data Env = Env
    { envVal :: ValLookup
    , envFn :: FnLookup
    }



-- Evaluation monad
--newtype Eval a = Eval { unEval :: ReaderT Env (ExceptT T.Text IO) a }
--    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError T.Text, MonadIO)

newtype Eval a = Eval { unEval :: ReaderT Env IO a }
  deriving ( Monad
           , Functor
           , Applicative
           , MonadReader Env
           , MonadIO)



