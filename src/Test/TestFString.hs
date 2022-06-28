{-# Language QuasiQuotes #-}

module Test.TestFString where

import Test.Test
import Format.FString



testCases :: [TestCase]
testCases
  = [ ( [fstring||],                          "",           "[fstring||]"                         )
    , ( [fstring|abc|],                       "abc",        "[fstring|abc|]"                      )
    , ( [fstring|{"abc"}|],                   "abc",        "[fstring|{\"abc\"}|]"                )
    , ( let x = "abc" in [fstring|{x}|],      "abc",        "let x = \"abc\" in [fstring|{x}|]"   )
    , ( let x = 1 :: Int in [fstring|{x}|],   "1",          "let x = 1 :: Int in [fstring|{x}|]"  )
    , ( [fstring|{"abc"%}|],                  "abc",        "[fstring|{\"abc\"%}|]"               )
    , ( [fstring|---{"XXX"}---|],             "---XXX---",  "[fstring|---{\"XXX\"}---|]"          )
    , ( [fstring|{"asd"}-{123 :: Int%}|],     "asd-123",    "[fstring|{\"asd\"}-{123}|]"          )
    
    , ( [fstring|{"abc"%?}|],   "\"abc\"", "[fstring|{\"abc\"%?}|] " )
    , ( [fstring|{"abc"%x?}|],  "\"abc\"", "[fstring|{\"abc\"%x?}|]" )
    , ( [fstring|{"abc"%X?}|],  "\"abc\"", "[fstring|{\"abc\"%X?}|]" )
    
    , ( [fstring|{1  :: Int}|],    "1",      "[fstring|{1}|]"     )
    , ( [fstring|{2  :: Int%b}|],  "10",     "[fstring|{2%b}|]"   )
    , ( [fstring|{8  :: Int%o}|],  "10",     "[fstring|{8%o}|]"   )
    , ( [fstring|{16 :: Int%x}|],  "10",     "[fstring|{16%x}|]"  )
    , ( [fstring|{16 :: Int%X}|],  "10",     "[fstring|{16%X}|]"  )
    , ( [fstring|{15 :: Int%x}|],  "f",      "[fstring|{15%x}|]"  )
    , ( [fstring|{15 :: Int%X}|],  "F",      "[fstring|{15%X}|]"  )
    , ( [fstring|{10 :: Int%e}|],  "1.0e1",  "[fstring|{10%e}|]"  )
    , ( [fstring|{10 :: Int%E}|],  "1.0E1",  "[fstring|{10%E}|]"  )
    , ( [fstring|{2  :: Int%#b}|], "0b10",   "[fstring|{2%b}|]"   )
    , ( [fstring|{8  :: Int%#o}|], "0o10",   "[fstring|{8%#o}|]"  )
    , ( [fstring|{16 :: Int%#x}|], "0x10",   "[fstring|{16%#x}|]" )
    , ( [fstring|{16 :: Int%#X}|], "0x10",   "[fstring|{16%#X}|]" )
    , ( [fstring|{15 :: Int%#x}|], "0xf",    "[fstring|{15%#x}|]" )
    , ( [fstring|{15 :: Int%#X}|], "0xF",    "[fstring|{15%#X}|]" )
    
    , ( [fstring|{1 :: Int%>3}|],                        "  1", "[fstring|{1%>3}|]"                        )
    , ( [fstring|{1 :: Int%^3}|],                        " 1 ", "[fstring|{1%^3}|]"                        )
    , ( [fstring|{1 :: Int%<3}|],                        "1  ", "[fstring|{1%<3}|]"                        )
    , ( [fstring|{1 :: Int%->3}|],                       "--1", "[fstring|{1%->3}|]"                       )
    , ( [fstring|{1 :: Int%-^3}|],                       "-1-", "[fstring|{1%-^3}|]"                       )
    , ( [fstring|{1 :: Int%-<3}|],                       "1--", "[fstring|{1%-<3}|]"                       )
    , ( [fstring|{1 :: Int%3}|],                         "1  ", "[fstring|{1%3}|]"                         )
    , ( let width = 3 in [fstring|{1 :: Int%<width$}|],  "1  ", "let width = 3 in [fstring|{1%<width$}|]"  )

    , ( [fstring|{1.0 :: Double}|],                             "1.0",      "[fstring|{}|]         1.0"                   )
    , ( [fstring|{1.0 :: Double%.2}|],                          "1.00",     "[fstring|{%.2}|]      1.0"                   )
    , ( [fstring|{1.001 :: Double%.2}|],                        "1.00",     "[fstring|{%.2}|]      1.001"                 )
    , ( let prec = 2 in [fstring|{1.0 :: Double%.prec$}|],      "1.00",     "let p = 2 in [fstring|{1.0%.prec$}|]"        )
    , ( let p = 2; w = 8 in [fstring|{1.0 :: Double%<w$.p$}|],  "1.00    ", "let p = 2; w = 8 in [fstring|{1.0%<w$.p$}|]" )
    ]
