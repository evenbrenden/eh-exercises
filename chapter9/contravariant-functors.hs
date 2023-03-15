-- Contravariant Functors

class ContraVariant f where
    contramap :: (b -> a) -> f a -> f b

newtype ContraFunction a b = ContraFunction (b -> a)

instance ContraVariant (ContraFunction a) where
    contramap f (ContraFunction g) = ContraFunction (g . f)
