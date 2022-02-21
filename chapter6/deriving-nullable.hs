{-# LANGUAGE DerivingVia #-}

class Nullable a where
    isNull :: a -> Bool
    nulll :: a

newtype Probably a = Probably (Maybe a)

instance Nullable a => Nullable (Probably a) where
    isNull (Probably Nothing) = True
    isNull (Probably Just{} ) = False
    nulll = Probably Nothing

newtype Presumably a = Presumably (Maybe a)

instance Nullable a => Nullable (Presumably a) where
    isNull (Presumably Nothing ) = True
    isNull (Presumably (Just a)) = isNull a
    nulll = Presumably Nothing

newtype Possibly a = Possibly (Maybe a)
    deriving Nullable via (Probably a)

newtype Perhaps a = Perhaps (Maybe a)
    deriving Nullable via (Presumably a)
