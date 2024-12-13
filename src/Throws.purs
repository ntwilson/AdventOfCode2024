module Throws where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Error, error)

-- if I evaluate Day6 Puzzle1 in the Either monad, the stack blows up.  Aff is inherently stack safe.
-- I'm creating this Throws class so that I can have Either String and Aff unify and use Either String
-- in tests, but then use Aff and just throw Errors when running on the real input. This seems simpler
-- than trampolining the whole thing.
class Throws :: Type -> (Type -> Type) -> Constraint
class Throws e m where
  throw :: ∀ a. e -> m a

instance Throws e (Either e) where
  throw = throwError

instance Throws String Aff where
  throw = error >>> throwError

instance Throws Error Aff where
  throw = throwError

liftMaybe :: ∀ f e a. Applicative f => Throws e f => e -> Maybe a -> f a
liftMaybe e = case _ of 
  Just x -> pure x
  Nothing -> throw e

liftEither :: ∀ f e a. Applicative f => Throws e f => Either e a -> f a
liftEither = case _ of
  Right x -> pure x
  Left e -> throw e

