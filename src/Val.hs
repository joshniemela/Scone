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
    (List l) -> T.concat ["(", T.unwords $ showScheme <$> l, ")"]
    (Function _) -> "<function>"
    (Lambda _ _) -> "<lambda>"

data SchemeException
    = NumArgs Integer [Scheme]
    | TypeMismatch T.Text Scheme
    | BadSpecialForm T.Text
    | ExpectedList Scheme
    | NotFunction Scheme
    | UnboundVar T.Text
    | Default T.Text
    deriving (Show)



newtype Fn = Fn { fn :: [Scheme] -> Eval Scheme }

type ValLookup = M.Map T.Text Scheme
type FnLookup = M.Map T.Text Fn

data Env = Env
    { envVal :: ValLookup
    , envFn :: FnLookup
    } deriving (Eq)



-- Evaluation monad
--newtype Eval a = Eval { unEval :: ReaderT Env (ExceptT T.Text IO) a }
--    deriving (Functor, Applicative, Monad, MonadReader Env, MonadError T.Text, MonadIO)

newtype Eval a = Eval { unEval :: ReaderT Env IO a }
  deriving ( Monad
           , Functor
           , Applicative
           , MonadReader Env
           , MonadIO)



