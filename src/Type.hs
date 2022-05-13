{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Type
  ( -- * Types
    LispVal (..),
    Eval (..),
    Env,
  )
where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT (ReaderT))
import qualified Data.Map as M
import qualified Data.Text as T


data LispVal
  = Symbol T.Text
  | Number Float
  | String T.Text
  | Function Func
  | Lambda Func Env
  | Nil
  | List [LispVal]

-- Represents a function which takes a list of @LispVal@'s and returns an @Eval@ computation
-- which evaluates a resulting @LispVal@.
newtype Func = Func {func :: [LispVal] -> Eval LispVal}

-- The environment
type Env = M.Map T.Text LispVal

-- Eval Monad
-- TODO: Use StateT instead of ReaderT to allow writing to the environment
newtype Eval a = Eval {unEval :: ReaderT Env IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader Env,
      MonadIO
    )

-- * Show instances
unwordsList :: [LispVal] -> T.Text
unwordsList xs = T.unwords $ T.pack . show <$> xs

instance Show LispVal where
  show (Symbol s) = show s
  show (Number n) = show n
  show (String s) = show s -- TODO: remove start/end quotes
  show (List l) = T.unpack $ T.concat ["(", unwordsList l, ")"]
  show (Function f) = "<function>"
  show (Lambda f env) = "<lambda>"
  show Nil = "<nil>"