{-# LANGUAGE OverloadedStrings #-}

module Interpreter
where

import Type

-- | Evaluate a @LispVal@ in the Eval monad
eval :: LispVal -> Eval LispVal
eval (Number n) = pure $ Number n
eval (String s) = pure $ String s

eval (List [Symbol "quote", val]) = pure val
eval _ = error "I don't know how to evaluate that type"