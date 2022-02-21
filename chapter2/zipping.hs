sipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
sipWith _ _         []        = []
sipWith _ []        _         = []
sipWith f (a1 : a2) (b1 : b2) = f a1 b1 : sipWith f a2 b2

zippo :: (a -> b -> c) -> [a] -> [b] -> [c]
zippo f as bs = [ f a b | (a, b) <- zip as bs ]

sipowicz :: (a -> b -> c) -> [a] -> [b] -> [c]
sipowicz f as bs = foldl (\abs (a, b) -> abs <> [f a b]) [] (zip as bs)
