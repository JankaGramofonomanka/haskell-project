module Display.Padding where


import Format.Parse (Align(..))

addPadding :: Char -> Int -> Align -> String -> String
addPadding fill width align s = let
    
    pad = (`replicate` fill)

    leftover = max 0 (width - length s)
    padL = (leftover `div` 2)
    padR = leftover - padL
  
  in case align of
    L -> pad leftover ++  s
    M -> pad padL     ++  s ++ pad padR
    R ->                  s ++ pad leftover
    



