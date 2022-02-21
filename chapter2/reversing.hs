reverze :: [a] -> [a]
reverze = foldr (\a bs -> bs <> [a]) mempty

rewerse :: [a] -> [a]
rewerse = foldl (\bs a -> a : bs) mempty
