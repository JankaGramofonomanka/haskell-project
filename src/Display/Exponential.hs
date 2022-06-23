module Display.Exponential where

import Data.Char

import Numeric


class DisplayExp a where
  displayExp :: a -> String

instance DisplayExp Float where
  displayExp x = showEFloat Nothing x ""

instance DisplayExp Double where
  displayExp x = showEFloat Nothing x ""

instance DisplayExp Int where
  displayExp x = showEFloat Nothing (fromIntegral x) ""

instance DisplayExp Integer where
  displayExp x = showEFloat Nothing (fromIntegral x) ""


displayExpU :: DisplayExp a => a -> String
displayExpU = map toUpper . displayExp
