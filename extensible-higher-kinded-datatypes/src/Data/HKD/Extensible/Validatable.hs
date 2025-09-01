module Data.HKD.Extensible.Validatable
  (module Data.HKD.Extensible.Validatable
  , module X) where

-- base
import Data.Bifunctor (Bifunctor (first))
import Data.Functor.Identity as X (Identity (Identity))
import Data.Kind (Type)

-- validation
import Data.Validation as X (Validation (..))

-- extensible-higher-hinded-datatypes
import Data.HKD.Extensible

data Validatable = Unvalidated | Validating Type | Validated Type | Correct

newtype Validator e a = Validator {getValidator :: a -> Maybe e}

valid :: Validator e a
valid = Validator $ const Nothing

type instance Interpret Validatable 'Unvalidated = Identity
type instance Interpret Validatable ('Validating e) = Validator e
type instance Interpret Validatable ('Validated e) = Validation e
type instance Interpret Validatable 'Correct = Identity

class ValidatableField e a where
  type UnvalidatedFieldType a :: Type
  validateField :: UnvalidatedFieldType a -> Validation e a

collectValidationP :: PhaseTraversal (Validatable ': aspects) ('Validated e '::: phases) ('Correct '::: phases) (Validation e)
collectValidationP = HeadTraversal $ fmap Identity

collectValidation :: (TraversableE d, Semigroup e) => d ('Validated e '::: phases) -> Validation e (d ('Correct '::: phases))
collectValidation = traverseHead $ fmap Identity

mapFailure :: (FunctorE d) => (e1 -> e2) -> d ('Validated e1 '::: phases) -> d ('Validated e2 '::: phases)
mapFailure f = mapHead $ first f

validateP :: PhaseZip (Validatable ': aspects) ('Unvalidated '::: phases) ('Validating e '::: phases) ('Validated e '::: phases)
validateP = HeadZip $ \(Identity unvalidated) (Validator f) -> maybe (Success unvalidated) Failure $ f unvalidated

validate :: ApplicativeE d => d ('Unvalidated '::: phases) -> d ('Validating e '::: phases) -> d ('Validated e '::: phases)
validate = zipWithHead $ \(Identity unvalidated) (Validator f) -> maybe (Success unvalidated) Failure $ f unvalidated
