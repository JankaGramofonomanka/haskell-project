{-# LANGUAGE FlexibleInstances #-}

module Display.Precision where

import Numeric



class DisplayPrecision a where
  displayPrec :: Int -> a -> String

--instance RealFloat a => DisplayPrecision a where
--  displayPrec n x = showFFloat (Just n) x ""
instance DisplayPrecision Float where
  displayPrec n x = showFFloat (Just n) x ""

instance DisplayPrecision Double where
  displayPrec n x = showFFloat (Just n) x ""

--instance Integral a => DisplayPrecision a where
--  displayPrec n x = showFFloat (Just n) (fromIntegral x) ""

instance DisplayPrecision Int where
  displayPrec n x = showFFloat (Just n) (fromIntegral x) ""

instance DisplayPrecision Integer where
  displayPrec n x = showFFloat (Just n) (fromIntegral x) ""


--displayPrec :: RealFloat a => Int -> a -> String
--displayPrec n x = showFFloat (Just n) x ""
