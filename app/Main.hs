{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Reader
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.Map as M
import Parser
import Types
import Interpreter (evalExpr)
main :: IO ()
main = do
  case whispRunParser "(quote (1 2 3))" of 
    Left e -> print $ errorBundlePretty e
    Right v -> do
      st <- evalExpr v
      print $ display st

  -- runReaderT act env