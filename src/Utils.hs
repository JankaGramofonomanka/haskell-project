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



cmp2 :: (a -> b -> c) -> (x -> a) -> (x -> b) -> x -> c
cmp2 f g h x = f (g x) (h x)

cmp3 :: (a -> b -> c -> d) -> (x -> a) -> (x -> b) -> (x -> c) -> x -> d
cmp3 f g h i x = cmp2 f g h x (i x)

cmp4 :: (a -> b -> c -> d -> e) -> (x -> a) -> (x -> b) -> (x -> c) -> (x -> d) -> x -> e
cmp4 f g h i j x = cmp3 f g h i x (j x)

cmp5 :: (a -> b -> c -> d -> e -> f) -> (x -> a) -> (x -> b) -> (x -> c) -> (x -> d) -> (x -> e) -> x -> f
cmp5 f g h i j k x = cmp4 f g h i j x (k x)
