{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader
import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Display (Display (..))
import Interpreter (evalExpr, evalFile)
import Parser
import Text.Megaparsec.Error (errorBundlePretty)
import Types

parseLine :: T.Text -> IO LispVal
parseLine file = case whispRunParser file of
  Left e -> error $ errorBundlePretty e
  Right v -> pure v

-- main :: IO ([LispVal], Env)
main :: IO (Either T.Text [LispVal], Env)
main = do
  file <- T.pack <$> readFile "test.wh"
  lines' <- traverse parseLine . T.lines $ file
  res <- evalFile lines'

  traverse_ (putStrLn . T.unpack . display) res
  case fst res of
    Left err -> T.putStrLn $ "error: " <> err
    Right v -> T.putStrLn $ "=> " <> foldMap display (filter (not . isNil) v)

  pure res