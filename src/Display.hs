{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Display
  ( Display (..),
    unwordsList
  )
where

import qualified Data.Map as M
import qualified Data.Text as T
import Types

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
  display Nil = "nil"

-- Display instances for the Environment
instance Display Env where
  display env = "{" <> (T.intercalate ", " . map display . M.assocs $ env) <> "\n}"

instance Display (T.Text, LispVal) where
  display (key, val) = "\n  " <> key <> " = " <> display val

instance Display (LispVal, Env) where
  display (l, env) = "{" <> display env <> "}"
