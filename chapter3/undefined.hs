-- Because undefined can represent any type, there are infinitely many variations.
addThree :: Int -> Int -> Int -> Int
addThree x y z = x `undefined` y `undefined` z
