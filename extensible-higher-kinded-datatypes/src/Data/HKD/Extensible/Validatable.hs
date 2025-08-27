module Data.HKD.Extensible.Validatable where

-- base
import Data.Bifunctor (Bifunctor (first))
import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)

-- validation
import Data.Validation (Validation)

-- extensible-higher-hinded-datatypes
import Data.HKD.Extensible

data Validatable = Unvalidated | Validated Type | Correct

type instance Interpret Validatable 'Unvalidated = Identity
type instance Interpret Validatable ('Validated e) = Validation e
type instance Interpret Validatable 'Correct = Identity

class ValidatableField e a where
  type UnvalidatedFieldType a :: Type
  validateField :: UnvalidatedFieldType a -> Validation e a

collectValidation :: (TraversableE d, Semigroup e) => d ('Validated e '::: phases) -> Validation e (d ('Correct '::: phases))
collectValidation = traverseHead $ fmap Identity

mapFailure :: (FunctorE d) => (e1 -> e2) -> d ('Validated e1 '::: phases) -> d ('Validated e2 '::: phases)
mapFailure f = mapHead $ first f
