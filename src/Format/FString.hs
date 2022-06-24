module Format.FString
  ( fstring
  , addPadding
  , Display(..)
  , DisplayBase(..)
  , displayBin
  , displayOct
  , displayHex
  , displayHexU
  , displayBinP
  , displayOctP
  , displayHexP
  , displayHexUP
  , DisplayPrecision(..)
  , DisplayExp(..)
  , displayExpU
  , Align(..)
  )
  where

import qualified Data.Map as M
import Data.Maybe
import Data.Either
import Data.Functor.Identity (Identity (runIdentity))

import Control.Monad.State.Strict

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Regex.Applicative
import Language.Haskell.Meta.Parse


import Display.Display
import Display.Padding
import Display.Base
import Display.Precision
import Display.Exponential


import Format.Parse
import Format.Common
import Utils



numberedArgError :: a
numberedArgError = error "positioned parameter doesn't make sense here"


instance FormatBuilder Identity where
  
  processArgument arg@(Named name) = return $ mkVarE name
  processArgument arg@(Numbered n) = numberedArgError


parseFString :: String -> Exp
parseFString s = let 
    items = fromMaybe (error "parse error") (s =~ stringWithBrackets)
    processedItems = map processItem items

    processItem :: Either String InsideBrackets -> Exp
    processItem (Left s) = mkStrLitE s
    processItem (Right (InsideBrackets s)) = let
        (exptS, specS) = splitOnLast '%' s
        exp = unsafeParseExp exptS
        spec = fromMaybe (error "cannot parse format spec") (specS =~ formatSpec)
      
      in runIdentity (processFormatSpec exp spec)

    mkConcat :: [Exp] -> Exp
    mkConcat items = foldl append' (mkStrLitE "") items

    append' :: Exp -> Exp -> Exp
    append' acc elem = UInfixE acc (mkVarE "++") elem

  in mkConcat processedItems

splitOnLast :: Char -> String -> (String, String)
splitOnLast ch s = case break (== ch) (reverse s) of
  (pre, "") -> (reverse pre, "")
  (pre, _ : post) -> (reverse post, reverse pre)

unsafeParseExp :: String -> Exp
unsafeParseExp s = case parseExp s of
  Right exp -> exp
  Left err -> error $ "cannot parse expression: `" ++ err ++ "`"



fstring :: QuasiQuoter
fstring = QuasiQuoter {
  quoteExp = pure . parseFString
}
