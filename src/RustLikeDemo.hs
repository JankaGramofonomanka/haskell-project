{-# LANGUAGE QuasiQuotes #-}

module RustLikeDemo where


import Format.RustLike


-- Ambiguous type variable
-- s0 = [rustFMT|{}|] 1
-- s0func = [rustFMT|{}|]

-- "1"
s1 = [rustFMT|{}|] (1 :: Int)
s1func = [rustFMT|{}|] :: Int -> String


-- "12"
s2 = [rustFMT|{}{}|] (1 :: Int) (2 :: Int)

-- "12"
s3 = [rustFMT|{0}{1}|] (1 :: Int) (2 :: Int)

-- "21"
s4 = [rustFMT|{1}{0}|] (1 :: Int) (2 :: Int)

-- "11"
s5 = [rustFMT|{0}{0}|] (1 :: Int)




x, y :: Integer
x = 1
y = 2



-- "--1"
s6 = [rustFMT|{:->3}|] (1 :: Int)


z :: Double
z = 1.2345

-- "1.23"
s7 = [rustFMT|{:.2}|] z


w :: Integer
w = 31

-- "11111"
s8 = [rustFMT|{:b}|] w

-- "0b11111"
s9 = [rustFMT|{:#b}|] w


-- "0x1F"
s10 = [rustFMT|{:#X}|] w



width = 10
prec = 4
-- "-200.0000-"
s11 = [rustFMT|{0:-^1$.2$}|] (100*y) width prec
s12 = [rustFMT|{1:-^2$.0$}|] prec (100*y) width


-- "lalala"
s13 = [rustFMT|{}|] "lalala"


-- "--lalala--"
s14 = [rustFMT|{:-^10}|] "lalala"

-- "abc|-0xBB8--|def|..lalala31|ghi"
s15 = [rustFMT|abc|{:-^#8X}|def|{:.>2$}|ghi|] (1000*(x + y)) ("lalala" ++ show w) width








