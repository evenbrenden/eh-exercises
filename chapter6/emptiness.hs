import           Data.Maybe

class Nullable a where
    isNull :: a -> Bool
    nulll :: a

instance Nullable a => Nullable (Maybe a) where
    isNull = isNothing
    nulll  = Nothing

instance (Nullable a, Nullable b) => Nullable (a, b) where
    isNull (a, b) = isNull a && isNull b
    nulll = (nulll, nulll)

instance Nullable [a] where
    isNull = null
    nulll  = []
