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

data Map :: (a -> ReturnValue b) -> f a -> ReturnValue (f b)
type instance Eval (Map f []) = []
type instance Eval (Map f (a : as)) = Eval (f a) : Eval (Map f as)

-- > :kind! Eval (Map Even [0,1,2,3,4])
-- Eval (Map Even [0,1,2,3,4]) :: [Bool]
-- = 'True : 'False : 'True : 'False : 'True : Eval (Map Even '[])

data Add :: Nat -> Nat -> ReturnValue Nat
type instance Eval (Add a b) = a + b

-- > :kind! Eval (Map (Add 2) [0,1,2,3,4])
-- Eval (Map (Add 2) [0,1,2,3,4]) :: [Nat]
-- = 2 : 3 : 4 : 5 : 6 : Eval (Map (Add 2) '[])
