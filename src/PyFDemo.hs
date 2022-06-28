{-# LANGUAGE QuasiQuotes #-}

module PyFDemo where


import PyF


-- Ambiguous type variable
--s0 = [fmt|{1}|]

-- Ambiguous type variable, but this is an example from the tutorial
-- s1 = [fmt|{-3:=6}|]

-- unexpected ':'
--s2 = [fmt|{1 :: Int}|]


x, y :: Integer
x = 1
y = 2

-- "3"
s3 = [fmt|{x + y}|]

-- "--1"
s4 = [fmt|{x:->3}|]

z :: Double
z = 1.2345

-- "1.23"
s5 = [fmt|{z:.2}|]

w :: Integer
w = 31

-- "11111"
s6 = [fmt|{w:b}|]

-- "0b11111"
s7 = [fmt|{w:#b}|]

-- "0X1F"
s8 = [fmt|{w:#X}|]

-- "1_1111_0000"
s9 = [fmt|{16*w:_b}|]

-- unexpected '{', but this is an example from the tutorial
-- s10 = [fmt|{1086:^{2 * 10}d}|]

-- "lalala"
s11 = [fmt|{"lalala"}|]

-- "--lalala--"
s12 = [fmt|{"lalala":-^10}|]







