{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HasNamedTypeOpenTypeFams where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type family NamedType (a :: Type) :: Symbol
type instance NamedType Int = "Int"
type instance NamedType Char = "Char"
type instance NamedType (a -> b) = NamedType a `AppendSymbol` " -> " `AppendSymbol` NamedType b
type instance NamedType [a] = "[" :++: NamedType a :++: "]"
type instance NamedType String = "[" :++: NamedType Char :++: "]"

showTypeName :: forall t. (KnownSymbol (NamedType t)) => String
showTypeName = symbolVal $ Proxy @(NamedType t)

type (:++:) a b = AppendSymbol a b

type instance
    NamedType (a, b) =
        "(" :++: NamedType a :++: "," :++: NamedType b :++: ")"
