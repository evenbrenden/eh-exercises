{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypeFamilyListFuncs where

import Data.Kind
import GHC.TypeLits

type ReturnValue r = r -> Type

data LessThanOrEqual :: Nat -> Nat -> ReturnValue Bool
data Even :: Nat -> ReturnValue Bool

type family Eval (expr :: ReturnValue r) :: r
type instance Eval (LessThanOrEqual a b) = b <=? a
type instance Eval (Even n) = EQ 0 (n `Mod` 2)

type family EQ (a :: k) (b :: k) :: Bool where
    EQ a a = True
    EQ a b = False

type family IfThenElse (p :: Bool) (t :: a) (f :: a) :: a where
    IfThenElse True t _ = t
    IfThenElse False _ f = f

type family FindElems (p :: a -> ReturnValue Bool) (elems :: [a]) :: [a] where
    FindElems _ '[] = '[]
    FindElems p (a : as) = IfThenElse (Eval (p a)) (a : FindElems p as) (FindElems p as)

-- Type Level map

type family Map (ab :: a -> ReturnValue b) (elems :: f a) :: f b where
    Map _ '[] = '[]
    Map f (a : as) = Eval (f a) : Map f as

data Add :: Nat -> Nat -> ReturnValue Nat

type instance Eval (Add a b) = a + b
