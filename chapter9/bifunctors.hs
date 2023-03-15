-- Bifunctors

class Bifunctor f where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

instance Bifunctor Either where
    bimap f g (Left x) = Left (f x)
    bimap f g (Right x) = Right (g x)
