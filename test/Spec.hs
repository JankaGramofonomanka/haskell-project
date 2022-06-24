
import Control.Monad

import Test.Test
import qualified Test.TestRustFMT as Rust
import qualified Test.TestFString as F




test :: TestCase -> IO ()
test (actual, expected, toPrint) = when (actual /= expected) $ do 
    putStrLn ("| " ++ toPrint ++ ":")
    putStrLn ("|   expected: " ++ expected)
    putStrLn ("|   actual:   " ++ actual)
    


main :: IO ()
main = putStrLn "rustFMT:"
    >> mapM_ test Rust.testCases
    >> putStrLn "fstring:"
    >> mapM_ test F.testCases
    >> putStrLn "Finished."
