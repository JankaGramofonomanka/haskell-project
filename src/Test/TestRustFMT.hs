{-# Language QuasiQuotes #-}

module Test.TestRustFMT where

import Test.Test
import Format.RustLike


testCases :: [TestCase]
testCases
  = [ ( [rustFMT||],                "",           "[rustFMT||]              " )
    , ( [rustFMT|abc|],             "abc",        "[rustFMT|abc|]           " )
    , ( [rustFMT|{}|]   "abc",      "abc",        "[rustFMT|{}|]   \"abc\"    "  )
    , ( [rustFMT|{:}|]  "abc",      "abc",        "[rustFMT|{:}|]  \"abc\"    "  )
    , ( [rustFMT|---{}---|] "XXX",  "---XXX---",  "[rustFMT|---{}---|] \"XXX\""  )

    , ( [rustFMT|{}-{}|]    "asd" (123 :: Int), "asd-123", "[rustFMT|{}-{}|]    \"asd\" 123" )
    , ( [rustFMT|{0}-{1}|]  "asd" (123 :: Int), "asd-123", "[rustFMT|{0}-{1}|]  \"asd\" 123" )
    , ( [rustFMT|{1}-{0}|]  "asd" (123 :: Int), "123-asd", "[rustFMT|{1}-{0}|]  \"asd\" 123" )

    , ( [rustFMT|{:?}|]   "abc", "\"abc\"", "[rustFMT|{:?}|]   \"abc\" " )
    , ( [rustFMT|{:x?}|]  "abc", "\"abc\"", "[rustFMT|{:x?}|]  \"abc\" " )
    , ( [rustFMT|{:X?}|]  "abc", "\"abc\"", "[rustFMT|{:X?}|]  \"abc\" " )
    
    , ( [rustFMT|{}|]   (1  :: Int), "1",     "[rustFMT|{}|]   1 " )
    , ( [rustFMT|{:b}|] (2  :: Int), "10",    "[rustFMT|{:b}|] 2 " )
    , ( [rustFMT|{:o}|] (8  :: Int), "10",    "[rustFMT|{:o}|] 8 " )
    , ( [rustFMT|{:x}|] (16 :: Int), "10",    "[rustFMT|{:x}|] 16" )
    , ( [rustFMT|{:X}|] (16 :: Int), "10",    "[rustFMT|{:X}|] 16" )
    , ( [rustFMT|{:x}|] (15 :: Int), "f",     "[rustFMT|{:x}|] 15" )
    , ( [rustFMT|{:X}|] (15 :: Int), "F",     "[rustFMT|{:X}|] 15" )
    , ( [rustFMT|{:e}|] (10 :: Int), "1.0e1", "[rustFMT|{:e}|] 10" )
    , ( [rustFMT|{:E}|] (10 :: Int), "1.0E1", "[rustFMT|{:E}|] 10" )
    , ( [rustFMT|{:#b}|] (2  :: Int), "0b10", "[rustFMT|{:b}|] 2 " )
    , ( [rustFMT|{:#o}|] (8  :: Int), "0o10", "[rustFMT|{:#o}|] 8 " )
    , ( [rustFMT|{:#x}|] (16 :: Int), "0x10", "[rustFMT|{:#x}|] 16" )
    , ( [rustFMT|{:#X}|] (16 :: Int), "0x10", "[rustFMT|{:#X}|] 16" )
    , ( [rustFMT|{:#x}|] (15 :: Int), "0xf",  "[rustFMT|{:#x}|] 15" )
    , ( [rustFMT|{:#X}|] (15 :: Int), "0xF",  "[rustFMT|{:#X}|] 15" )
    
    , ( [rustFMT|{:<3}|]  (1 :: Int),   "  1", "[rustFMT|{:<3}|]  1   " )
    , ( [rustFMT|{:^3}|]  (1 :: Int),   " 1 ", "[rustFMT|{:^3}|]  1   " )
    , ( [rustFMT|{:>3}|]  (1 :: Int),   "1  ", "[rustFMT|{:>3}|]  1   " )
    , ( [rustFMT|{:-<3}|] (1 :: Int),   "--1", "[rustFMT|{:-<3}|] 1   " )
    , ( [rustFMT|{:-^3}|] (1 :: Int),   "-1-", "[rustFMT|{:-^3}|] 1   " )
    , ( [rustFMT|{:->3}|] (1 :: Int),   "1--", "[rustFMT|{:->3}|] 1   " )
    , ( [rustFMT|{:3}|]   (1 :: Int),   "1  ", "[rustFMT|{:3}|]   1   " )
    , ( [rustFMT|{:>1$}|] (1 :: Int) 3, "1  ", "[rustFMT|{:>1$}|] 1 3 " )

    , ( [rustFMT|{}|]         (1.0 :: Double),      "1.0",      "[rustFMT|{}|]         1.0    " )
    , ( [rustFMT|{:.2}|]      (1.0 :: Double),      "1.00",     "[rustFMT|{:.2}|]      1.0    " )
    , ( [rustFMT|{:.2}|]      (1.001 :: Double),    "1.00",     "[rustFMT|{:.2}|]      1.001  " )
    , ( [rustFMT|{:.1$}|]     (1.0 :: Double) 2,    "1.00",     "[rustFMT|{:.1$}|]     1.0 2  " )
    , ( [rustFMT|{:>1$.2$}|]  (1.0 :: Double) 8 2,  "1.00    ", "[rustFMT|{:>1$.2$}|]  1.0 8 2" )
    ]
