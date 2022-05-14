{-# LANGUAGE OverloadedStrings #-}

module Interpreter (evalExpr) where

import Control.Monad.State
import Types

-- | Evaluate a @LispVal@ in the Eval monad
eval :: LispVal -> Eval LispVal
eval (Number n) = pure $ Number n
eval (String s) = pure $ String s
eval (List (l : rest)) = case l of
  Symbol "def" -> evalDef l rest
  Symbol "quote" -> pure . head $ rest
  _ -> error "Invalid operation on list"
-- No idea how to evaluate that type
eval _ = error "I don't know how to evaluate that type"

evalDef :: LispVal -> [LispVal] -> Eval LispVal
evalDef name val = undefined

evalExpr expr = runStateEnv primEnv $ eval expr

runStateEnv :: Env -> Eval a -> IO (a, Env)
runStateEnv env action = runStateT (unEval action) env