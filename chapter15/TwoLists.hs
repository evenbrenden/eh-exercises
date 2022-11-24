{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TwoLists where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

data TwoLists :: [Symbol] -> [Nat] -> Type where
    Empty :: TwoLists '[] '[]
    Add ::
        (KnownSymbol x, KnownNat y) =>
        TwoLists xs ys ->
        TwoLists (x : xs) (y : ys)

class
    Viewable
        a
    where
    view :: a -> String

-- The point I am trying to make here is that the type checker will catch this
-- instance Viewable (TwoLists ss ns) => Viewable (TwoLists (s : ss) ns) where
--     view (Add xs) = symbolVal (Proxy @s) <> view xs
instance Viewable (TwoLists ss ns) => Viewable (TwoLists (s : ss) (n : ns)) where
    view (Add xs) = symbolVal (Proxy @s) <> show (natVal (Proxy @n)) <> view xs

instance Viewable (TwoLists '[] '[]) where
    view Empty = mempty

list = Add @"a" @1 $ Add @"b" @2 $ Add @"c" @3 Empty

main :: IO ()
main = print $ view list
