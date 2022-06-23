module Utils where



revApp :: [a] -> [a] -> [a]
revApp xs l = foldl (flip (:)) l xs




splitOnElem :: Eq a => a -> [a] -> [[a]]
splitOnElem _ []  =  []
splitOnElem e s   =  cons (
  case break (== e) s of
    (l, s') -> (l, case s' of
                    []      -> []
                    _:s''   -> splitOnElem e s'')
  )
  where
    cons ~(h, t)        =  h : t


comp1 :: (a -> b) -> (x -> a) -> x -> b
comp1 = (.)

comp2 :: (a -> b -> c) -> (x -> a) -> (x -> b) -> x -> c
comp2 f g h x = comp1 f g x (h x)

comp3 :: (a -> b -> c -> d) -> (x -> a) -> (x -> b) -> (x -> c) -> x -> d
comp3 f g h i x = comp2 f g h x (i x)

comp4 :: (a -> b -> c -> d -> e) -> (x -> a) -> (x -> b) -> (x -> c) -> (x -> d) -> x -> e
comp4 f g h i j x = comp3 f g h i x (j x)

comp5 :: (a -> b -> c -> d -> e -> f) -> (x -> a) -> (x -> b) -> (x -> c) -> (x -> d) -> (x -> e) -> x -> f
comp5 f g h i j k x = comp4 f g h i j x (k x)
