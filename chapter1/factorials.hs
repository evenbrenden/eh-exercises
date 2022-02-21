factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 1
factorial n = n * factorial (n - 1)
