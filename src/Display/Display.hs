{-# LANGUAGE FlexibleInstances #-}

module Display.Display where

import Data.Kind


class Display a where
  display :: a -> String

instance Display String where
  display = id

instance Display Int where
  display = show

instance Display Integer where
  display = show

instance Display Double where
  display = show

instance Display Float where
  display = show



