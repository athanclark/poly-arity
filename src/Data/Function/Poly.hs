{-# LANGUAGE
    TypeFamilies
  , KindSignatures
  , DataKinds
  , ConstraintKinds
  , TypeOperators
  #-}

module Data.Function.Poly where

import Data.Constraint


type family TypeListToArity (xs :: [*]) (r :: *) :: * where
  TypeListToArity '[] r = r
  TypeListToArity (x ': xs) r = x -> TypeListToArity xs r

type family ExpectArity (xs :: [*]) (f :: *) :: Constraint where
  ExpectArity '[] f = () -- basis
  ExpectArity (x ': xs) (x -> remainder) = ExpectArity xs remainder

-- class ConsumeArity
