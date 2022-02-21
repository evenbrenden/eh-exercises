konkatMap :: (a -> [b]) -> [a] -> [b]
konkatMap f = foldr (\a bs -> f a <> bs) []

katkonMap :: (a -> [b]) -> [a] -> [b]
katkonMap f = foldl (\bs a -> bs <> f a) []
