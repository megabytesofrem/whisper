{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Reader
import qualified Data.Map as M
import Type

act :: ReaderT Env IO ()
act = do
  res <- asks (M.lookup "test")
  liftIO $ print res

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  -- runReaderT act env