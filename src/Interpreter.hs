{-# LANGUAGE OverloadedStrings #-}

module Interpreter
  ( evalExpr,
    evalFile,
  )
where

import Control.Monad.Except
import Control.Monad.State
  ( -- Monad transformers
    MonadState (get),
    StateT (runStateT),
    modify,
  )
import qualified Data.Map as M
import qualified Data.Text as T
import Display (Display (..))
import Types

-- | Evaluate a @LispVal@ in the Eval monad
eval :: LispVal -> Eval LispVal
eval (Number n) = pure $ Number n
eval (String s) = pure $ String s
eval (List (l : rest)) = case l of
  Symbol "def" -> evalDef (display . head $ rest) $ tail rest
  Symbol "quote" -> pure . head $ rest
  Symbol s -> evalSymbol s
  _ -> throwError "Expected a symbol as the first item of a list"
-- No idea how to evaluate that
eval _ = throwError "I don't know how to evaluate that"

evalSymbol :: T.Text -> Eval LispVal
evalSymbol s = do
  env <- get
  case env M.!? s of
    Nothing -> throwError "Could not find that symbol in the environment"
    Just v -> pure v

-- (def name value)
evalDef :: T.Text -> [LispVal] -> Eval LispVal
evalDef name args = do
  env <- get

  -- Trim the quotes and convert from String -> Data.Text
  let name' = T.takeWhile (/= '"') . T.drop 1 $ name
  modify (M.insert name' (head args))
  pure Nil

evalExpr :: LispVal -> IO (Either T.Text LispVal, Env)
evalExpr = runStateEnv primEnv . eval

evalFile :: Traversable t => t LispVal -> IO (Either T.Text (t LispVal), Env)
evalFile = runStateEnv primEnv . mapM eval

-- | Wrapper around @runStateT@ that takes an environment and action to run
--   it in.
-- runStateEnv :: Env -> Eval a -> IO (Either T.Text (a, Env))
runStateEnv :: Env -> Eval a -> IO (Either T.Text a, Env)
runStateEnv env action = runStateT (runExceptT $ unEval action) env