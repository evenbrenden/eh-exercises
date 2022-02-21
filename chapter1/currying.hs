kurry :: ((a, b) -> c) -> (a -> b -> c)
kurry abc = \a b -> abc (a, b)

unkurry :: (a -> b -> c) -> ((a, b) -> c)
unkurry abc = \(a, b) -> abc a b
