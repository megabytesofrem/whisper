{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( -- * Types
    LispVal (..),
    Eval (..),
    Env,
    Display (..),
    primEnv,
  )
where

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

unwordsList :: [LispVal] -> T.Text
unwordsList xs = T.unwords $ display <$> xs

-- | Typeclass for user displayable strings, since @Show@ formats internal
--   representations.
class Display a where
  -- | Similar to @show@ but only for user displayable strings
  display :: a -> T.Text

instance Display LispVal where
  display (Symbol s) = T.pack . show $ s
  display (Number n) = T.pack . show $ n
  display (String s) = T.pack . show $ s
  display (List l) = T.concat ["(", unwordsList l, ")"]
  display (Function f) = "<function>"
  display (Lambda f env) = "<lambda>"
  display Nil = "<nil>"

-- Display instances for the Environment
instance Display Env where
  -- TODO: Implement @Display@ for @Env@
  display = T.intercalate ", " . map display . M.assocs

instance Display (T.Text, LispVal) where
  display (key, val) = key <> " = " <> display val

instance Display (LispVal, Env) where
  display (l, env) = "{" <> display env <> "}"

-- Represents a function which takes a list of @LispVal@'s and returns an @Eval@ computation
-- which evaluates a resulting @LispVal@.
newtype Func = Func {func :: [LispVal] -> Eval LispVal}

-- The environment
type Env = M.Map T.Text LispVal

primEnv :: Env
primEnv =
  M.fromList
    [ ("PI", Number 3.14),
      ("test", String "hello world")
    ]

-- Eval Monad
-- TODO: Use StateT instead of ReaderT to allow writing to the environment
newtype Eval a = Eval {unEval :: StateT Env IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadState Env,
      MonadIO
    )
