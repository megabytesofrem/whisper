{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( -- * Types
    LispVal (..),
    Eval (..),
    Env,
    isNil,
    primEnv,
  )
where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State
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

-- | Return whether or not the @LispVal@ is Nil
isNil :: LispVal -> Bool
isNil Nil = True
isNil _ = False

-- | Represents a function which takes a list of @LispVal@'s and returns an @Eval@ computation
--   which evaluates a resulting @LispVal@.
newtype Func = Func {func :: [LispVal] -> Eval LispVal}

-- The environment
type Env = M.Map T.Text LispVal

primEnv :: Env
primEnv =
  M.fromList
    [ ("PI", Number 3.14),
      ("abc", String "hello world")
    ]

-- Eval Monad
-- TODO: Use StateT instead of ReaderT to allow writing to the environment
newtype Eval a = Eval {unEval :: ExceptT T.Text (StateT Env IO) a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadState Env,
      MonadError T.Text,
      MonadIO
    )
