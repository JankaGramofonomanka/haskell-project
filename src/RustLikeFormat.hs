module RustLikeFormat
  ( formatString
  , formatSpec
  , FormatString(..)
  , Format(..)
  , Ident
  , Arg(..)
  , Argument(..)
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


import Data.Char ( isAlpha, isDigit )

import Text.Regex.Applicative
import Text.Regex.Applicative.Common

import Utils

eps :: RE s ()
eps = mempty

option :: RE s a -> a -> RE s a
option try except = try <|> (except <$ eps)

optionDflt :: Default a => RE s a -> RE s a
optionDflt try = option try dflt


class Default a where
  dflt :: a

newtype FormatString = FormatString [Either String Format] deriving (Show, Eq, Ord)
formatString :: RE Char FormatString
formatString = FormatString <$> ((:) <$> text <*> items) where
  
  items :: RE Char [Either String Format]
  items = concat <$> many item

  item :: RE Char [Either String Format]
  item = l2 <$> maybeFormat <*> text

  l2 :: a -> a -> [a]
  l2 x y = [x, y]
  
  text :: RE Char (Either String Format)
  text = Left <$> noBrackets

  maybeFormat :: RE Char (Either String Format)
  maybeFormat = Left "{" <$ string "\\{" <|> Left "}" <$ string "\\}" <|> Right <$> format

noBrackets :: RE Char String
noBrackets = many $ psym (`notElem` ['{', '}'])

data Format = Format Arg FormatSpec deriving (Show, Eq, Ord)
format :: RE Char Format
format = Format <$> (sym '{' *> arg) <*> optionDflt (sym ':' *> formatSpec) <* sym '}'

type Ident = String
ident :: RE Char Ident
ident = (:) <$> psym isAlpha <*> many isOk where
  isOk :: RE Char Char
  isOk = psym isAlpha <|> psym isDigit <|> sym '_'

data Arg = Specified Argument | ArgFromInput deriving (Show, Eq, Ord)
instance Default Arg where
  dflt = ArgFromInput

arg :: RE Char Arg
arg = optionDflt (Specified <$> argument)


data Argument = Numbered Int | Named Ident deriving (Show, Eq, Ord)
argument :: RE Char Argument
argument = Named <$> ident <|> Numbered <$> decimal
--newtype Argument = Numbered Int deriving (Show, Eq, Ord)
--argument :: RE Char Argument
--argument = Numbered <$> decimal



data Padding = Padding Fill Width Align deriving (Show, Eq, Ord)
instance Default Padding where
  dflt = Padding dflt dflt dflt

data FormatSpec
  = FormatSpec
  { _padding      :: Padding
  , _sign         :: Maybe Sign
  , _hash         :: Bool
  , _zero         :: Bool
  , _precision    :: Maybe Precision
  , _displayType  :: DisplayType
  }
  deriving (Show, Eq, Ord)
instance Default FormatSpec where
  dflt = FormatSpec dflt Nothing False False Nothing dflt

fromFormatSpec' :: FormatSpec' -> FormatSpec
fromFormatSpec' spec' = let
  padding = comp3 Padding _fill' _width' _align' spec'

  in comp5 (FormatSpec padding) _sign' _hash' _zero' _precision' _displayType' spec'

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
  deriving (Show, Eq, Ord)

formatSpec' :: RE Char FormatSpec'
formatSpec' = comp2 FormatSpec' fst snd <$> fillAndAlign
                                        <*> sign'
                                        <*> hash
                                        <*> zero
                                        <*> width'
                                        <*> precision'
                                        <*> displayType

  where

    fillAndAlign :: RE Char (Fill, Align)
    fillAndAlign = option ((,) <$> optionDflt fill <*> align) (dflt, dflt)

    sign' = option (Just <$> sign) Nothing
    hash = option (True <$ sym '#') False
    zero = option (True <$ sym '0') False
    width' = optionDflt width
    precision' = option (sym '.' *> (Just <$> precision)) Nothing



newtype Fill = Fill Char deriving (Show, Eq, Ord)
instance Default Fill where
  dflt = Fill ' '

fill :: RE Char Fill
fill = Fill <$> anySym


data Align = L | M | R deriving (Show, Eq, Ord)
instance Default Align where
  dflt = R

align :: RE Char Align
align = L <$ sym '<' <|> M <$ sym '^' <|> R <$ sym '>'


data Sign = Plus | Minus deriving (Show, Eq, Ord)
sign :: RE Char Sign
sign = Plus <$ sym '+' <|> Minus <$ sym '-'


newtype Width = Width Count deriving (Show, Eq, Ord)
instance Default Width where
  dflt = Width (Fixed 0)

width :: RE Char Width
width = Width <$> count

data Precision = Prec Count | PrecFromInput deriving (Show, Eq, Ord)
precision :: RE Char Precision
precision = Prec <$> count <|> PrecFromInput <$ sym '*'

data UL = Lower | Upper deriving (Show, Eq, Ord)
data DisplayType
  = UseDisplay
  | UseShow
  | UseOctal
  | UseHex UL
  | UseBinary
  | UseExp UL
  deriving (Show, Eq, Ord)

instance Default DisplayType where
  dflt = UseDisplay

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


data Count = Fixed Integer | Variable Parameter deriving (Show, Eq, Ord)
count :: RE Char Count
count = Fixed <$> decimal <|> Variable <$> parameter

newtype Parameter = Parameter Argument deriving (Show, Eq, Ord)
parameter :: RE Char Parameter
parameter = Parameter <$> argument  <* sym '$'















