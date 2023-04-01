module Eval  where

import Val ( Env(..), Scheme(..) )
import qualified Data.Map as M
import qualified Data.Text as T
import Prim (primEnv)


funcEnv :: M.Map T.Text Scheme
funcEnv = M.fromList primEnv

basicEnv :: Env
basicEnv = Env M.empty funcEnv

