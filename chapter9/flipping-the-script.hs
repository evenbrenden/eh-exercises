-- Flipping the Script
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use <&>" #-}

data List a = Empty | List a (List a)

concatList Empty as = as
concatList (List a as) bs = List a (concatList as bs)

instance Monad List where
    return a = List a Empty
    Empty >>= f = Empty
    List a as >>= f = concatList (f a) (as >>= f)

instance Applicative List where
    pure a = List a Empty
    Empty <*> _ = Empty
    List f fs <*> vals = concatList (vals >>= return . f) (fs <*> vals)

instance Functor List where
    fmap _ Empty = Empty
    fmap f (List a as) = List (f a) (pure f <*> as)
