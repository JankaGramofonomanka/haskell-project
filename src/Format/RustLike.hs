{-# LANGUAGE RecordWildCards, FlexibleInstances #-}

module Format.RustLike
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


import Format.Parse
import Format.Common
import Utils


namedArgError :: a
namedArgError = error "named parameter doesn't make sense here"


mkParamName :: Int -> String
mkParamName n = "x" ++ show n


type ArgNum = Int
data FunState = FunState { implicitArgNum :: ArgNum, numParams :: Int } deriving Show
initFunState :: FunState
initFunState = FunState 0 0

instance FormatBuilder (State FunState) where
  
  processArgument arg@(Named _) = namedArgError
  processArgument arg@(Numbered n) = do
    addParam n
    let paramName = mkParamName n
    return $ VarE (mkName paramName)

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
  toFormat <- mkParamName <$> getArgNum arg
  processFormatSpec (mkVarE toFormat) spec

getArgNum :: Arg -> State FunState ArgNum
getArgNum (Specified (Numbered n)) = addParam n >> return n
getArgNum (Specified (Named _)) = namedArgError
getArgNum ArgFromInput = do
  n <- gets implicitArgNum
  incrImplicitArgNum
  return n

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
