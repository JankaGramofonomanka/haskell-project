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



displayBin, displayOct, displayHex, displayHexU :: DisplayBase a => a -> String
displayBin = displayBase 2
displayOct = displayBase 8
displayHex = displayBase 16
displayHexU = map toUpper . displayHex

displayBinP, displayOctP, displayHexP, displayHexUP :: DisplayBase a => a -> String
displayBinP = ("0b" ++) . displayBin
displayOctP = ("0o" ++) . displayOct
displayHexP = ("0x" ++) . displayHex
displayHexUP = ("0x" ++) . displayHexU
