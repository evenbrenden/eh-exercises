{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module PeanoClosedTypeFams where

import GHC.TypeLits

data Peano = Zero | Succ Peano

type family ToPeano (n :: Nat) :: Peano where
    ToPeano 0 = Zero
    ToPeano a = Succ (ToPeano (a - 1))

type family FromPeano (a :: Peano) :: Nat where
    FromPeano Zero = 0
    FromPeano (Succ a) = 1 + FromPeano a

type family Add (a :: Peano) (b :: Peano) :: Peano where
    Add a Zero = a
    Add a (Succ b) = Add (Succ a) b

type family Subtract (a :: Peano) (b :: Peano) :: Peano where
    Subtract a Zero = a
    Subtract Zero b =
        TypeError
            ( Text "Subtract cannot result in a negative number"
                :$$: Text "The result would be -" :<>: ShowType (FromPeano b)
            )
    Subtract (Succ a) (Succ b) = Subtract a b

type family Multiply (a :: Peano) (b :: Peano) :: Peano where
    Multiply a Zero = Zero
    Multiply a (Succ Zero) = a
    Multiply a (Succ b) = Add a (Multiply a b)
