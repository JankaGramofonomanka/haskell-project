{-# LANGUAGE RecordWildCards #-}

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


import Format.RustLikeFormat
import Utils



numberedArgError :: a
numberedArgError = error "positioned parameter doesn't make sense here"

addPaddingE :: Exp
addPaddingE = mkVarE "addPadding"

displayE :: Exp
displayE = mkDisplayExpr ""

displayPrecE :: Exp
displayPrecE = mkDisplayExpr "Prec"

displayExpE :: Exp
displayExpE = mkDisplayExpr "Exp"

displayExpUE :: Exp
displayExpUE = mkDisplayExpr "ExpU"

displayBinE :: Exp
displayBinE = mkDisplayExpr "Bin"

displayOctE :: Exp
displayOctE = mkDisplayExpr "Oct"

displayHexE :: Exp
displayHexE = mkDisplayExpr "Hex"

displayHexUE :: Exp
displayHexUE = mkDisplayExpr "HexU"

displayBinPE :: Exp
displayBinPE = mkDisplayExpr "BinP"

displayOctPE :: Exp
displayOctPE = mkDisplayExpr "OctP"

displayHexPE :: Exp
displayHexPE = mkDisplayExpr "HexP"

displayHexUPE :: Exp
displayHexUPE = mkDisplayExpr "HexUP"


showE :: Exp
showE = mkVarE "show"

mkVarE :: String -> Exp
mkVarE s = VarE (mkName s)

mkLitIntE :: Integer -> Exp
mkLitIntE = LitE . IntegerL

mkStrLitE :: String -> Exp
mkStrLitE = LitE . StringL

mkDisplayExpr :: String -> Exp
mkDisplayExpr s = mkVarE ("display" ++ s)

mkAlignE :: Align -> Exp
mkAlignE L = ConE $ mkName "L"
mkAlignE M = ConE $ mkName "M"
mkAlignE R = ConE $ mkName "R"

mkParamName :: Int -> String
mkParamName n = "x" ++ show n

mkApp :: Exp -> [Exp] -> Exp
mkApp = foldl AppE



processFormatSpec :: Exp -> FormatSpec -> Identity Exp
processFormatSpec toFormat spec = do
  displayExp <- processDispl spec toFormat
  processPadding (_padding spec) displayExp


processDispl :: FormatSpec -> Exp -> Identity Exp
processDispl spec toFormat = do

  let displType = _displayType spec
  let prec = _precision spec
  let hash = _hash spec

  mPrecE <- processPrec prec
  
  (func, args) <- pure $ case displType of
    UseDisplay -> case mPrecE of
      Nothing -> (displayE, [toFormat])
      Just precE -> (displayPrecE, [precE, toFormat])

    UseShow       -> (showE,                                          [toFormat])
    UseOctal      -> (if hash then displayOctPE   else displayOctE,   [toFormat])
    UseHex Lower  -> (if hash then displayHexPE   else displayHexE,   [toFormat])
    UseHex Upper  -> (if hash then displayHexUPE  else displayHexUE,  [toFormat])
    UseBinary     -> (if hash then displayBinPE   else displayBinE,   [toFormat])
    UseExp Lower  -> (displayExpE,                                    [toFormat])
    UseExp Upper  -> (displayExpUE,                                   [toFormat])
  
  return $ mkApp func args

processPrec :: Maybe Precision -> Identity (Maybe Exp)
processPrec Nothing = return Nothing
processPrec (Just PrecFromInput) = undefined
processPrec (Just (Prec (Fixed n))) = return $ Just (mkLitIntE n)
processPrec (Just (Prec (Variable (Parameter arg)))) = Just <$> processArgument arg




processPadding :: Padding -> Exp -> Identity Exp
processPadding (Padding _ (Width (Fixed 0)) _) toPad = return toPad
processPadding (Padding (Fill fill) width align) toPad = do
  w <- processWidth width
  return $ mkApp addPaddingE [LitE $ CharL fill, w, mkAlignE align, toPad]

processWidth :: Width -> Identity Exp
processWidth (Width (Fixed n)) = return $ mkLitIntE n
processWidth (Width (Variable (Parameter arg))) = processArgument arg

processArgument :: Argument -> Identity Exp
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
