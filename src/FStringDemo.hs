{-# LANGUAGE QuasiQuotes #-}

module FStringDemo where


import Format.FString


-- Ambiguous type variable
--s0 = [fstring|{1}|]

s1 = [fstring|{1 :: Int}|]


x, y :: Integer
x = 1
y = 2

-- "3"
s2 = [fstring|{x + y}|]

-- "--1"
s3 = [fstring|{x%->3}|]

z :: Double
z = 1.2345

-- "1.23"
s4 = [fstring|{z%.2}|]

w :: Integer
w = 31

-- "11111"
s5 = [fstring|{w%b}|]

-- "0b11111"
s6 = [fstring|{w%#b}|]

-- "0x1F"
s7 = [fstring|{w%#X}|]


width = 10
prec = 4
-- "-200.0000-"
s8 = [fstring|{100*y%-^width$.prec$}|]

-- "lalala"
s9 = [fstring|{"lalala"}|]

-- "--lalala--"
s10 = [fstring|{"lalala"%-^10}|]

-- "abc|-0xBB8--|def|..lalala31|ghi"
s11 = [fstring|abc|{1000*(x + y)%-^#8X}|def|{"lalala" ++ show w%.>width$}|ghi|]





