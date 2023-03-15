-- Profunctors

class Profunctor f where
    dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

-- We cannot make a Profunctor for Function nor ContraFunction, because we need both a covariant function and a contravariant function to complete the square f a b -> f c d.
