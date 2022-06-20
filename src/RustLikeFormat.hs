module RustLikeFormat
  ( formatSpec
  , Ident
  , Argument
  , Padding(..)
  , FormatSpec(..)
  , Fill(..)
  , Align(..)
  , Sign(..)
  , Width(..)
  , Precision(..)
  , UL(..)
  , DisplayType(..)
  , Count(..)
  , Parameter(..)
  ) where


import Data.Char

import Text.Regex.Applicative
import Text.Regex.Applicative.Common

import Utils

eps :: RE s ()
eps = mempty

option :: RE s a -> a -> RE s a
option try except = try <|> (except <$ eps)
option' :: RE s a -> a -> RE s a
option' try except = (except <$ eps) <|> try


type Ident = String
ident :: RE Char Ident
ident = (:) <$> psym isAlpha <*> many isOk where
  isOk :: RE Char Char
  isOk = psym isAlpha <|> psym isDigit <|> sym '_'

data Argument = Numbered Int | Named Ident deriving Show
argument :: RE Char Argument
argument = Named <$> ident <|> Numbered <$> decimal



data Padding = Padding Fill Width Align deriving Show
data FormatSpec
  = FormatSpec
  { _padding      :: Padding
  , _sign         :: Maybe Sign
  , _hash         :: Bool
  , _zero         :: Bool
  , _precision    :: Maybe Precision
  , _displayType  :: DisplayType
  }
  deriving Show

fromFormatSpec' :: FormatSpec' -> FormatSpec
fromFormatSpec' spec' = let
  padding = cmp3 Padding _fill' _width' _align' spec'

  in cmp5 (FormatSpec padding) _sign' _hash' _zero' _precision' _displayType' spec'

formatSpec :: RE Char FormatSpec
formatSpec = fromFormatSpec' <$> formatSpec'

data FormatSpec'
  = FormatSpec'
  { _fill'        :: Fill
  , _align'       :: Align
  , _sign'        :: Maybe Sign
  , _hash'        :: Bool
  , _zero'        :: Bool
  , _width'       :: Width
  , _precision'   :: Maybe Precision
  , _displayType' :: DisplayType
  }
  deriving Show

formatSpec' :: RE Char FormatSpec'
formatSpec' = cmp2 FormatSpec' fst snd  <$> fillAndAlign
                                        <*> sign'
                                        <*> hash
                                        <*> zero
                                        <*> width'
                                        <*> precision'
                                        <*> displayType

  where

    fillAndAlign :: RE Char (Fill, Align)
    fillAndAlign = option ((,) <$> option fill (Fill ' ') <*> align) (Fill ' ', R)

    sign' = option (Just <$> sign) Nothing
    hash = option (True <$ sym '#') False
    zero = option (True <$ sym '0') False
    width' = option width (Width $ Fixed 0)
    precision' = option (sym '.' *> (Just <$> precision)) Nothing



newtype Fill = Fill Char deriving Show
fill :: RE Char Fill
fill = Fill <$> anySym

data Align = L | M | R deriving Show
align :: RE Char Align
align = L <$ sym '<' <|> M <$ sym '^' <|> R <$ sym '>'

data Sign = Plus | Minus deriving Show
sign :: RE Char Sign
sign = Plus <$ sym '+' <|> Minus <$ sym '-'

newtype Width = Width Count deriving Show
width :: RE Char Width
width = Width <$> count

data Precision = Prec Count | PrecFromInput deriving Show
precision :: RE Char Precision
precision = Prec <$> count <|> PrecFromInput <$ sym '*'

data UL = Lower | Upper deriving Show
data DisplayType = UseDisplay | UseShow | UseOctal | UseHex UL | UseBinary | UseExp UL deriving Show
displayType :: RE Char DisplayType
displayType = UseDisplay    <$ eps
          <|> UseShow       <$ sym '?'
          <|> UseShow       <$ string "x?"
          <|> UseShow       <$ string "X?"
          <|> UseOctal      <$ sym 'o'
          <|> UseHex Lower  <$ sym 'x'
          <|> UseHex Upper  <$ sym 'X'
          <|> UseBinary     <$ sym 'b'
          <|> UseExp Lower  <$ sym 'e'
          <|> UseExp Upper  <$ sym 'E'


data Count = Fixed Int | Variable Parameter deriving Show
count :: RE Char Count
count = Fixed <$> decimal <|> Variable <$> parameter

newtype Parameter = Parameter Argument deriving Show
parameter :: RE Char Parameter
parameter = Parameter <$> argument  <* sym '$'















