poly-arity
==========

> Tools for working with functions of undetermined arity.

Most of these tools are type-level constructs to ensure that your function types
will have a certain initial shape, because the function arrow `->` itself makes
a type-level list, where each `(->)` would be like `(:)`.

## Usage

Your first try might look something like this:

```haskell
{-# LANGUAGE
    TypeFamilies
  , TypeOperators
  , DataKinds
  , PolyKinds
  , ConstraintKinds
  , KindSignatures
  , AllowAmbiguousTypes
  #-}

module Example where

import Data.Function.Poly
import Data.HList


type family (xs :: [k]) :++ (ys :: [k]) :: [k] where
  '[]       :++ ys = ys
  (x ': xs) :++ ys = x ': xs :++ ys

foo :: ( TypeListToArity xs r ~ f -- Proves the arity - list isomorphism
       , ArityToTypeList f ~ (:++) xs (r ': '[])
       ) => f -> f
foo = id

bar :: ( ArityMinusTypeList f xs ~ r -- Proves replacement
       , ExpectArity xs f
       ) => f -> f
bar = id

-- warning - the above don't actually work :\

xs :: HList (Integer ': Bool ': Maybe () ': '[])
xs = HCons (5 :: Integer) $ HCons True $ HCons (Just ()) HNil


baz :: Integer -> Bool -> Maybe () -> String -> ()
baz _ _ _ _ = ()
```

then, we can toy with `appN` in `ghci`:

```haskell
λ> :t baz `appN` xs
baz `appN` xs :: String -> ()
```
