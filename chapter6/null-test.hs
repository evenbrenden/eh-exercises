class Eq a => Nullable a where
    isNull :: a -> Bool
    nulll :: a
    isNull = (==) nulll
