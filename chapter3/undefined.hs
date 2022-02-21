addThree1 :: Int -> Int -> Int -> Int
addThree1 x y z = x + y + z

addThree2 :: Int -> Int -> Int -> Int
addThree2 x y z = x + y + undefined

addThree3 :: Int -> Int -> Int -> Int
addThree3 x y z = x + undefined + z

addThree4 :: Int -> Int -> Int -> Int
addThree4 x y z = x + undefined + undefined

addThree5 :: Int -> Int -> Int -> Int
addThree5 x y z = undefined + y + z

addThree6 :: Int -> Int -> Int -> Int
addThree6 x y z = undefined + y + undefined

addThree7 :: Int -> Int -> Int -> Int
addThree7 x y z = undefined + undefined + z

addThree8 :: Int -> Int -> Int -> Int
addThree8 x y z = undefined + undefined + undefined
