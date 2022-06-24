{-# LANGUAGE RecordWildCards #-}

module RustLikeFormatTH
  ( rustFMT
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

import Control.Monad.State.Strict

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Regex.Applicative


import Display.Display
import Display.Padding
import Display.Base
import Display.Precision
import Display.Exponential


import RustLikeFormat
import Utils




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



type ArgNum = Int
data FunState = FunState { implicitArgNum :: ArgNum, numParams :: Int } deriving Show
initFunState :: FunState
initFunState = FunState 0 0

addParam :: ArgNum -> State FunState ()
addParam n = do
  FunState { numParams = k, .. } <- get
  put $ FunState { numParams = max k (n + 1), .. }

incrImplicitArgNum :: State FunState ()
incrImplicitArgNum = do
  FunState { implicitArgNum = n, .. } <- get
  put $ FunState { implicitArgNum = n + 1, .. }
  addParam n
  
  

processFormat :: Format -> State FunState Exp
processFormat (Format arg spec) = do
  toFormat <- mkName . mkParamName <$> getArgNum arg
  processFormatSpec toFormat spec

getArgNum :: Arg -> State FunState ArgNum
getArgNum (Specified (Numbered n)) = addParam n >> return n
getArgNum ArgFromInput = do
  n <- gets implicitArgNum
  incrImplicitArgNum
  return n

processFormatSpec :: Name -> FormatSpec -> State FunState Exp
processFormatSpec toFormat spec = do
  displayExp <- processDispl spec (VarE toFormat)
  processPadding (_padding spec) displayExp


processDispl :: FormatSpec -> Exp -> State FunState Exp
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

processPrec :: Maybe Precision -> State FunState (Maybe Exp)
processPrec Nothing = return Nothing
processPrec (Just PrecFromInput) = undefined
processPrec (Just (Prec (Fixed n))) = return $ Just (mkLitIntE n)
processPrec (Just (Prec (Variable (Parameter arg)))) = Just <$> processArgument arg




processPadding :: Padding -> Exp -> State FunState Exp
processPadding (Padding _ (Width (Fixed 0)) _) toPad = return toPad
processPadding (Padding (Fill fill) width align) toPad = do
  w <- processWidth width
  return $ mkApp addPaddingE [LitE $ CharL fill, w, mkAlignE align, toPad]

processWidth :: Width -> State FunState Exp
processWidth (Width (Fixed n)) = return $ mkLitIntE n
processWidth (Width (Variable (Parameter arg))) = processArgument arg

processArgument :: Argument -> State FunState Exp
processArgument arg@(Numbered n) = do
  addParam n
  let paramName = mkParamName n
  return $ VarE (mkName paramName)


processFormatString :: FormatString -> State FunState Exp
processFormatString (FormatString items) = mkConcat <$> mapM processItem items where
  processItem :: Either String Format -> State FunState Exp
  processItem (Left s) = return (mkStrLitE s)
  processItem (Right format) = processFormat format

  mkConcat :: [Exp] -> Exp
  mkConcat items = foldl append' (mkStrLitE "") items

  append' :: Exp -> Exp -> Exp
  append' acc elem = UInfixE acc (mkVarE "++") elem







mkFormatFunc :: FormatString -> Exp
mkFormatFunc fs = let
    (body, funState) = runState (processFormatString fs) initFunState
    n = numParams funState
    params = map (VarP . mkName . mkParamName) [0..(n-1)]

  in case params of
    [] -> body
    (_ : _) -> LamE params body




mkFormatStringFuncExp :: String -> Exp
mkFormatStringFuncExp s = mkFormatFunc $ fromMaybe (error "parse error") (s =~ formatString)



rustFMT :: QuasiQuoter
rustFMT = QuasiQuoter {
  quoteExp = pure . mkFormatStringFuncExp
}
