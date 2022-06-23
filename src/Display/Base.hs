{-# LANGUAGE FlexibleInstances #-}

module Display.Base where

import Numeric
import Data.Char


class DisplayBase a where
  displayBase :: Integer -> a -> String

instance DisplayBase Int where
  displayBase b x = showIntAtBase b intToDigit (fromIntegral x) ""

instance DisplayBase Integer where
  displayBase b x = showIntAtBase b intToDigit x ""

displayBaseRealFloat :: (RealFloat a, Show a) => Integer -> a -> String
displayBaseRealFloat b x = let
    (digits, e) = floatToDigits b x
    zeros = replicate (e - length digits) 0
    (pre, post) = splitAt e (digits ++ zeros)
  in map intToDigit pre ++ "." ++ map intToDigit post

instance DisplayBase Float where
  displayBase = displayBaseRealFloat

instance DisplayBase Double where
  displayBase = displayBaseRealFloat



displayBin, displayOct, displayHex :: DisplayBase a => a -> String
displayBin = displayBase 2
displayOct = displayBase 8
displayHex = displayBase 16
