svap :: (a, b) -> (b, a)
svap (a, b) = (b, a) -- Only possible implementation

concat :: Foldable t => t [a] -> [a]
concat = foldMap id -- One possible implementation

aid :: a -> a
aid a = a -- Only possible implementation
