{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Traversable where

import Course.Applicative
import Course.Compose
import Course.Core
import Course.ExactlyOne
import Course.Functor
import Course.List
import Course.Optional

{- | All instances of the `Traversable` type-class must satisfy three laws. These
laws are not checked by the compiler. These laws are given as:

* The law of naturality
  `∀f g. f . traverse g ≅ traverse (f . g)`

* The law of identity
  `∀x. traverse ExactlyOne x ≅ ExactlyOne x`

* The law of composition
  `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
-}
class (Functor t) => Traversable t where
  traverse ::
    (Applicative k) =>
    (a -> k b) ->
    t a ->
    k (t b)

instance Traversable List where
  traverse ::
    (Applicative k) =>
    (a -> k b) ->
    List a ->
    k (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    (Applicative k) =>
    (a -> k b) ->
    ExactlyOne a ->
    k (ExactlyOne b)
  traverse f a = ExactlyOne <$> f (runExactlyOne a)

instance Traversable Optional where
  traverse ::
    (Applicative k) =>
    (a -> k b) ->
    Optional a ->
    k (Optional b)
  traverse _ Empty = pure Empty
  traverse f (Full a) = Full <$> f a

{- | Sequences a traversable value of structures to a structure of a traversable value.

>>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
ExactlyOne [7,8,9]

>>> sequenceA (Full (ExactlyOne 7))
ExactlyOne (Full 7)

>>> sequenceA (Full (*10)) 6
Full 60
-}
sequenceA ::
  (Applicative k, Traversable t) =>
  t (k a) ->
  k (t a)
sequenceA = traverse id

instance
  (Traversable f, Traversable g) =>
  Traversable (Compose f g)
  where
  -- Implement the traverse function for a Traversable instance for Compose
  traverse ::
    (Traversable f, Traversable g, Applicative k) =>
    (a -> k b) ->
    Compose f g a ->
    k (Compose f g b)
  traverse f (Compose a) = Compose <$> traverse (traverse f) a

-- traverse f (Compose a) = Compose <$> _

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a
  = Product (f a) (g a)
  deriving (Show, Eq)

instance
  (Functor f, Functor g) =>
  Functor (Product f g)
  where
  -- Implement the (<$>) function for a Functor instance for Product
  (<$>) ::
    (Functor f, Functor g) =>
    (a -> b) ->
    Product f g a ->
    Product f g b
  f <$> (Product a b) = Product (f <$> a) (f <$> b)

instance
  (Traversable f, Traversable g) =>
  Traversable (Product f g)
  where
  -- Implement the traverse function for a Traversable instance for Product
  traverse ::
    (Traversable f, Traversable g, Applicative k) =>
    (a -> k b) ->
    Product f g a ->
    k (Product f g b)
  traverse f (Product a b) = Product <$> traverse f a <*> traverse f b

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a
  = InL (f a)
  | InR (g a)
  deriving (Show, Eq)

instance
  (Functor f, Functor g) =>
  Functor (Coproduct f g)
  where
  -- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) ::
    (Functor f, Functor g) =>
    (a -> b) ->
    Coproduct f g a ->
    Coproduct f g b
  f <$> InL a = InL (f <$> a)
  f <$> InR a = InR (f <$> a)

instance
  (Traversable f, Traversable g) =>
  Traversable (Coproduct f g)
  where
  -- Implement the traverse function for a Traversable instance for Coproduct
  traverse ::
    (Traversable f, Traversable g, Applicative k) =>
    (a -> k b) ->
    Coproduct f g a ->
    k (Coproduct f g b)
  traverse f (InL a) = InL <$> traverse f a
  traverse f (InR a) = InR <$> traverse f a
