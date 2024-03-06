{-# LANGUAGE DataKinds, KindSignatures, ScopedTypeVariables, TypeApplications #-}

module Util.Modular where


import GHC.TypeNats
import Data.Proxy

newtype Modular (n :: Nat) a = Modular a

instance (KnownNat n) => Show (Modular n Integer) where
  show (Modular x) = show x ++ " mod " ++ show n
    where n = fromIntegral (natVal (Proxy @n))

instance (KnownNat n) => Num (Modular n Integer) where
  fromInteger x = Modular (x `mod` n)
    where n = fromIntegral (natVal (Proxy @n))
  Modular x + Modular y = abs $ Modular (x + y `mod` n)
    where n = fromIntegral (natVal (Proxy @n))

  Modular x * Modular y = abs $ Modular (x * y `mod` n)
    where n = fromIntegral (natVal (Proxy @n))

  Modular x - Modular y = abs $ Modular (x - y `mod` n)
    where n = fromIntegral (natVal (Proxy @n))

  abs (Modular x) = Modular (if x > 0 then x else n - x)
    where n = fromIntegral (natVal (Proxy @n))

  signum = const 1

  negate = id

