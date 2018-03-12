{-# LANGUAGE TypeFamilies, TypeInType, TypeOperators, ConstraintKinds, 
             UndecidableInstances, GADTs, ScopedTypeVariables, TypeFamilyDependencies #-}

module Elektra.SpecElektra (
  SpecKey (..),  Spec (..),
  TySpec (..), TyKey(..), BaseType (..), CheckType (..),
  LftType (..), type ($), type (<=>), type (?>), --type (?+), type (<+>)
) where

import Data.Kind (type (*), Constraint)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Data.Type.Bool

-- Our "Types"
-- using the type family we can constrain what we can pass as a parameter
-- Specifically, we enforce
data CheckType = Any
               | forall a . a :?+ CheckType

data LftType = In | Out | Same

data BaseType = Key KeyType
              | BaseType :-> BaseType

data KeyType  = Top KeyType
              | Chk CheckType
              | Sum KeyType KeyType
              | Bot

type Path  = String
type Value = Maybe String
type TopUnit = ('Top $ 'Chk 'Any)

-- *** some funky type level operators that makes it look nicer and less parantheses ***

type f $ a = f a
infixr 2 $

type t <=> u = Intersection t u
infixl 3 <=>

type n ?> m = n $ 'Chk m
infixr 3 ?>

--type n ?+ m = n ':?+ 'ChkType m
--infixr 3 ?+

--type n <+> m = SubExpansion n ('ChkType m)
--infixr 3 <+>

--
-- *** Basic data and type definitions ***
--

-- The GADT that defines our keys and their properties in our type system
-- It is basically an alternative evaluation/representation approach compared to the function approach down below
data SpecKey (a :: BaseType) = SpecKey Path Value deriving Show

-- Our Specifications. We only allow functions specified on our keys to enforce our BaseTypes
-- Haskell Types are not allowed as we want to stay inside our BaseType Kind
-- But in order to allow the optional addition of being able to calculate default values via normal haskell functions
-- we just carry a second representation along. While the second representation would leak out haskell types and values
-- and thus does not match our type system specification, the first parameter does
data Spec (k :: BaseType) (b :: *)
  where
    Lft :: SpecKey a -> Spec a (SpecKey a)
    Lam :: (Spec a (SpecKey a) -> Spec c d) -> Spec (a ':-> c) (SpecKey a -> d)
    App :: Spec (a ':-> c) (b -> d) -> Spec a b -> Spec c d

data TyKey (a :: KeyType) = TyKey Path Value deriving Show
data TySpec (k :: BaseType)
  where
    TyLft :: TyKey a -> TySpec ('Key a)
    TyLam :: (TySpec a -> TySpec b) -> TySpec (a ':-> b)
    TyApp :: TySpec (a ':-> b) -> TySpec a -> TySpec b

-- This is not required for the type system, its an optional extension to be able to support haskell functions directly
-- to work with key values
reduce :: Spec a b -> b
reduce (Lft k)   = k
reduce (Lam fn)  = \(x :: SpecKey y) -> reduce (fn (Lft x))
reduce (App f x) = reduce f (reduce x)

eval :: (b ~ SpecKey ('Key c)) => Spec a b -> SpecKey ('Key (Erase c))
eval = (\(SpecKey p v) -> SpecKey p v) . reduce

tyEval :: TyKey a -> TyKey (Erase a)
tyEval = undefined
--
-- *** Our type system rules expressed in first order logic ***
--
--type family SubExpansion (s :: BaseType) c :: BaseType where
--  SubExpansion ('Top TopUnit) x = 'Chk x
--  SubExpansion ('Chk s)     x = 'Chk $ s ':?+ x
--  SubExpansion _              _ = 'Bot

-- Clears out the top/inferring states and resolves outstanding errors
-- This is used to cleanup logical properties which are only relevant to the type system implementation
type family Erase (a :: KeyType) :: KeyType where
  Erase ('Top TopUnit)    = 'Chk 'Any
  Erase ('Top a)          = a
  Erase ('Sum 'Bot _)     = 'Bot
  Erase ('Sum _ 'Bot)     = 'Bot
  Erase ('Sum a b)        = 'Sum (Erase a) (Erase b)
  Erase a                 = a

-- meaning of the rules from top to bottom:
  -- If the second one has no information yet, we ignore it and stick to the current one
  -- The first type has absolutely no information yet, so it will use the type of the second
  -- If both are top types, intersect them, it will be at least any
  -- Similar to the previous case where one key has already a fixed type
  -- Similar to the previous case where one key has already a fixed type 
  -- We can assign other Subtypes to Sub in case they match, otherwise its BOT
  -- If c is the type a or b, this sum type assignment works, otherwise not
  -- e.g. a = NotNull b, in case a and b match thats fine and a inherits the property
  -- e.g. NonWrite b = a, we can't assign anything to it
  -- e.g. Encrypted a = Encrypted b, in case they are compatible thats fine
  -- same types unify obviously
  -- We can assign everything to Any
  -- anything else is not inferable/compatible and will result in the invalid bottom type
type family (Intersection) (a :: KeyType) (b :: KeyType) :: KeyType where
  -- top cases / inference related
  Intersection ('Top TopUnit)     b                  = 'Top b
  Intersection a                  ('Top TopUnit)     = 'Bot
  Intersection ('Top a)           ('Top b)           = 'Top $ a <=> b
  Intersection ('Top a)           b                  = 'Top $ a <=> b
  Intersection a                  ('Top b)           = If (a == (a <=> b)) a 'Bot
  -- check types
  Intersection ('Chk a)           ('Chk b)           = 'Chk $ IntersectChecktype a b
  -- sum types
  Intersection ('Sum a b)         c                  = IntersectSumtype a b c 
  -- same types intersect too obviously
  Intersection a                  a                  = a
--  Intersection _                  _                  = 'Bot
  --TypeError (Text "Trying to fallback to a key with an incompatible type")

type family (a :: KeyType) == (b :: KeyType) :: Bool where
  a == a = 'True
  _ == _ = 'False

--type family Not (a :: Bool) :: Bool
--type instance Not 'True = 'False
--type instance Not 'False = 'True

--type family (a :: Bool) && (b :: Bool) :: Bool where
--  'True && 'True = 'True
--  _ && _         = 'False

--type family (a :: KeyType) || (b :: KeyType) :: KeyType where
--  'Bot || b    = b
--  a    || 'Bot = a
--  a    || a    = a
--  a    || 'Chk 'Any = a
-- _    || _    = 'Bot

type family (a :: KeyType) ?? (b :: KeyType) :: KeyType where
--  a ?? 'Bot = 'Bot
  a ?? b    = a

type family IntersectSumtype (a :: KeyType) (b :: KeyType) (c :: KeyType) :: KeyType where
  IntersectSumtype a          b          a          = 'Sum a b
  IntersectSumtype b          a          a          = 'Sum b a
--  IntersectSumtype ('Chk a)   ('Chk b) ('Chk c)     = 'Sum ('Chk a) ('Chk b) ?? (('Chk $ IntersectChecktype a c) || ('Chk $ IntersectChecktype b c))
  IntersectSumtype ('Chk a)   b        ('Chk c)     = 'Sum ('Chk a) b ?? ('Chk $ IntersectChecktype a c)
  IntersectSumtype a          ('Chk b) ('Chk c)     = 'Sum a ('Chk b) ?? ('Chk $ IntersectChecktype b c)

type family Elem a ys where
    Elem x (x ':?+ ys) = 'True
    Elem x (y ':?+ ys) = Elem x ys
    Elem x y           = 'False

--type family If (x :: Bool) t e where
--  If 'True t  _  = t
--  If 'False _ e = e

type family IntersectChecktype (xs :: CheckType) (ys :: CheckType) where
  IntersectChecktype 'Any ys = 'Any
  IntersectChecktype xs 'Any = 'Any
  IntersectChecktype (x ':?+ xs) (x ':?+ ys) = x ':?+ (IntersectChecktype xs ys)
  IntersectChecktype (x ':?+ xs) (y ':?+ ys) = If (Elem x ys)
                                                  (x ':?+ (IntersectChecktype xs (y ':?+ ys)))
                                                  (IntersectChecktype xs (y ':?+ ys))

data Test
data Test2

type Testz = IntersectChecktype 'Any (Test ':?+ 'Any)
type Testz2 = IntersectChecktype (Test ':?+ 'Any) 'Any
type Testz3 = IntersectChecktype (Test ':?+ 'Any) (Test2 ':?+ (Test ':?+ 'Any))
type Testz4 = IntersectChecktype (Test ':?+ (Test2 ':?+ 'Any)) (Test2 ':?+ (Test ':?+ 'Any))
--type TestTop0 = Intersection ('Top TopUnit) ('Top TopUnit)
--type TestTop1 = Intersection ('Top TopUnit) ('Chk Testz)
--type TestTop2 = Intersection ('Chk Testz3) ('Top TopUnit)
--type TestTop3 = Intersection ('Chk Testz3) ('Chk Testz4)
--type TestTop4 = Intersection ('Top $ 'Chk Testz4) ('Top $ 'Chk Testz3)

k1 :: TyKey ('Chk (Test2 ':?+ 'Any))
k1 = undefined


k2 :: TyKey ('Chk (Test2 ':?+ (Test ':?+ 'Any)))
k2 = undefined

checkInt :: TyKey ('Chk a) -> TyKey ('Chk (Int ':?+ a))
checkInt = undefined

type family (a :: KeyType) ? (b :: KeyType) :: Constraint where
  a ? b = (a <=> b) ~ a

data RestrictBinary
data Binary
data NonNull
--class Fallbk (a :: KeyType) (b :: KeyType) (c :: KeyType) (d :: *) where
--  fallbk :: TySpec ('Key a) -> TySpec ('Key b) -> TySpec ('Key $ c)
--instance (b ? a, ('Chk (Test ':?+ 'Any) ? b), (a <=> b) ~ blub) => Fallbk a b blub NonNull
--instance (b ? a) => Fallbk a b c Test

--anyKey :: TyKey ('Chk 'Any)
anyKey = undefined

noKey :: TyKey TopUnit
noKey = undefined

restrictBinary :: TyKey ('Chk a) -> TyKey ('Chk (RestrictBinary ':?+ a))
restrictBinary = undefined

checkBinary :: TyKey ('Chk a) -> TyKey ('Chk (Binary ':?+ a))
checkBinary = undefined

kb = checkBinary anyKey

type family FallbackResult (a :: KeyType) (b :: KeyType) :: KeyType where
  FallbackResult ('Chk a) ('Chk b) =
    If ((('Chk a) <=> ('Chk b)) == ('Chk b)) (TypeError ('Text "The two types don't unify: " ':<>: 'ShowType a ':<>: 'Text " falling back to " ':<>: 'ShowType b))
      (If (Elem NonNull b)
        ('Chk (NonNull ':?+ (IntersectChecktype a b)))
        (If (Elem Binary b && Elem RestrictBinary a) (TypeError ('Text "Cannot fallback to binary key"))
          ('Chk $ IntersectChecktype a b)))
  FallbackResult a b = a <=> b

krb = noKey
binaryRestricted = fallback krb kb

fallback :: TyKey a -> TyKey b -> TyKey (FallbackResult a b)
fallback = undefined

transTest2ToTest :: 'Chk (Int ':?+ 'Any) ? a => TyKey a -> TyKey ('Chk (Test ':?+ 'Any))
transTest2ToTest = undefined

fallbackTest = fallback k1 k2
--fallbackTest2 = fallback (TyLft k2) (TyLft k1)

test = transTest2ToTest $ checkInt k1

lk2 = TyLft k2
test2 = transTest2ToTest $ checkInt k2

data Bla a = Bla
