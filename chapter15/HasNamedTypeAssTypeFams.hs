{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HasNamedTypeAssTypeFams where

import GHC.TypeLits

class HasNamedType t where
    type NamedType t :: Symbol

instance HasNamedType Int where
    type NamedType Int = "Int"

instance HasNamedType Char where
    type NamedType Char = "Char"

instance HasNamedType String where
    type NamedType String = "String"

instance (HasNamedType a, HasNamedType b) => HasNamedType (a -> b) where
    type NamedType (a -> b) = NamedType a `AppendSymbol` " -> " `AppendSymbol` NamedType b
