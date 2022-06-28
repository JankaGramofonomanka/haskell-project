{-# LANGUAGE
    GADTs
  , DataKinds
  , KindSignatures
  , StandaloneKindSignatures
  , TypeApplications
  , ScopedTypeVariables
  , TypeFamilies
  , TypeOperators

  , TemplateHaskell
  , StandaloneDeriving
  , FlexibleInstances
#-}

module Printf where

import Data.Singletons
import Data.Singletons.TH
import Data.List.Singletons
import GHC.TypeLits.Singletons
import Data.Text

import Unsafe.Coerce


$(singletons [d|
  data Format
    = FInt Format
    | FString Format
    | FOther Char Format
    | FEnd
  |])

formatString :: String -> Format
formatString ('%' : 'd' : cs) = FInt (formatString cs)
formatString ('%' : 's' : cs) = FString (formatString cs)
formatString (c : cs)        = FOther c (formatString cs)
formatString []               = FEnd

type family FormatString (s :: String) :: Format where
  FormatString ('%' : 'd' : cs) = FInt (FormatString cs)
  FormatString ('%' : 's' : cs) = FString (FormatString cs)
  FormatString (c : cs)        = FOther c (FormatString cs)
  FormatString '[]              = FEnd

sFormatString :: Sing s -> Sing (FormatString s)
sFormatString (SCons sc1@(SChar :: SChar c1) sc2cs@(SCons (SChar :: SChar c2) scs))
  = case (charVal (Proxy :: Proxy c1), charVal (Proxy :: Proxy c2)) of
    ('%', 'd')  -> unsafeCoerce $ SFInt (sFormatString scs)
    ('%', 's')  -> unsafeCoerce $ SFString (sFormatString scs)
    (_, _)      -> unsafeCoerce $ SFOther sc1 (sFormatString sc2cs)

sFormatString (SCons sc@(SChar :: SChar c) scs) = unsafeCoerce $ SFOther sc (sFormatString scs)
sFormatString SNil = SFEnd
  

formatText :: Text -> Format
formatText = formatString . unpack

type family InterpFormat (format :: Format) :: * where
  InterpFormat (FInt f)     = Int -> InterpFormat f
  InterpFormat (FString f)  = String -> InterpFormat f
  InterpFormat (FOther _ f) = InterpFormat f
  InterpFormat FEnd         = String

toFunction :: Sing f -> String -> InterpFormat f
toFunction (SFInt sf) t       = \i -> toFunction sf (t ++ (show i))
toFunction (SFString sf) t    = \s -> toFunction sf (t ++ s)
toFunction (SFOther sc sf) t  = toFunction sf (t ++ [fromSing sc])
toFunction SFEnd t            = t

printf :: Sing s -> InterpFormat (FormatString s)
printf s = toFunction (sFormatString s) ""

-- Demo -----------------------------------------------------------------------
one       = printf (sing :: Sing ['%', 'd']) 1
_1_       = printf (sing :: Sing ['_', '%', 'd', '_']) 1
_abc_     = printf (sing :: Sing ['_', '%', 's', '_']) "abc"
_abc_cba_ = printf (sing :: Sing ['_', '%', 's', '_', '%', 's', '_']) "abc" "cba"

