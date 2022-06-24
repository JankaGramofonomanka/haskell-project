import Test

import Control.Monad

test :: TestCase -> IO ()
test (actual, expected, toPrint) = when (actual /= expected) $ print toPrint

main :: IO ()
main = mapM_ test testCases >> putStrLn "Finished."
