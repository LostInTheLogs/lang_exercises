{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.Compose where

import Course.Applicative
import Course.Contravariant
import Course.Core
import Course.Functor
import Course.Monad

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import Data.Set
import qualified Data.Set as S
import qualified Prelude as P

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a
  = Compose (f (g a))
  deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance
  (Functor f, Functor g) =>
  Functor (Compose f g)
  where
  (<$>) ::
    (Functor f, Functor g) =>
    (a -> b) ->
    Compose f g a ->
    Compose f g b
  f <$> (Compose a) = Compose $ (f <$>) <$> a

instance
  (Applicative f, Applicative g) =>
  Applicative (Compose f g)
  where
  -- Implement the pure function for an Applicative instance for Compose
  pure :: (Applicative f, Applicative g) => a -> Compose f g a
  pure = Compose . pure . pure

  -- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) ::
    (Applicative f, Applicative g) =>
    Compose f g (a -> b) ->
    Compose f g a ->
    Compose f g b
  Compose fgf <*> Compose fga = Compose $ (<*>) <$> fgf <*> fga

instance
  (Monad f, Monad g) =>
  Monad (Compose f g)
  where
  -- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "todo: Course.Compose (=<<)#instance (Compose f g)"

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
instance
  (Functor f, Contravariant g) =>
  Contravariant (Compose f g)
  where
  -- Implement the (>$<) function for a Contravariant instance for Compose
  (>$<) ::
    (Functor f, Contravariant g) =>
    (b -> a) ->
    Compose f g a ->
    Compose f g b
  f >$< Compose g = Compose $ (f >$<) <$> g
