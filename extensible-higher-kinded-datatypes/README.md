# Extensible Higher Kinded Datatypes

```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module README where

-- base
import Data.Char (isAlphaNum)
import Data.Function ((&))

-- extensible-higher-kinded-datatypes
import Data.HKD.Extensible
import Data.HKD.Extensible.Label
import Data.HKD.Extensible.Optionality
import Data.HKD.Extensible.Validatable

data Account (phases :: Phases '[Label String, Optionality, Validatable]) = Account
  { address :: Address (Head phases '::: (Head (Tail (Tail phases))) '::: 'Nil)
  , username :: Interpret (Label String) (Head phases) (Interpret Validatable (Head (Tail (Tail phases))) String)
  , realname :: Interpret (Label String) (Head phases) (Interpret Validatable (Head (Tail (Tail phases))) (Interpret Optionality (Head (Tail phases)) String))
  }

data Address (phases :: Phases '[Label String, Validatable]) = Address
  { street :: Interpret (Label String) (Head phases) (Interpret Validatable (Head (Tail phases)) String)
  , number :: Interpret (Label String) (Head phases) (Interpret Validatable (Head (Tail phases)) Housenumber)
  , country :: Interpret (Label String) (Head phases) (Interpret Validatable (Head (Tail phases)) String)
  }

newtype Housenumber = Housenumber Int
  deriving newtype (Eq, Ord, Num)

accountLabel :: Account ('Label '::: phases)
accountLabel = Account
  { address = Address
    { street = "The street you're currently living in"
    , number = "Your housenumber"
    , country = "Your Country of residence"
    }
  , username = "Username, must be lowercase alphanumerical"
  , realname = "Your real name"
  }

myRawAccount :: Account ('Value '::: 'Optional '::: 'Unvalidated '::: Nil)
myRawAccount = Account
  { address = Address
    { street = "Datalane"
    , number = 42
    , country = "HKD"
    }
  , username = "tyl"
  , realname = Identity $ Identity $ Just "Tyler"
  }

validator :: Account ('Value '::: 'Required '::: 'Validating String '::: Nil)
validator = Account
  { address = Address
      { street = Identity valid
      , number = Identity $ Validator $ \n -> if n > 0 then Nothing else Just "Must be positive"
      , country = Identity valid
      }
  , username = Identity $ Validator $ \un -> if all isAlphaNum un then Nothing else Just "Not alphanumeric"
  , realname = Identity valid
  }

validatedAccount :: Validation String (Account ('Value '::: 'Required '::: 'Correct '::: Nil))
validatedAccount =
    myRawAccount
  & phaseTraversal (TailTraversal requireP)
  & maybe (Failure "Optional value was not present")
    (\account -> phaseZip (TailZip (TailZip validateP)) account validator
    & phaseTraversal (TailTraversal (TailTraversal collectValidationP)))

instance ConstraintsE Account where
  type AllPhases c Account =
    ( c (Interpret (Label String) 'Value)
    , c (Interpret (Label String) 'Label)
    , c (Interpret Validatable 'Unvalidated)
    -- , (forall e . c (Interpret Validatable ('Validating e)))
    -- , (forall e . c (Interpret Validatable ('Validated e)))
    , c (Interpret Validatable 'Correct)
    )

instance AllPhases Functor Account => FunctorE Account where
  phaseMap (HeadMap f) Account {address, username, realname} = Account
    { address = phaseMap (HeadMap f) address
    , username = f username
    , realname = f realname
    }
  phaseMap (TailMap (HeadMap f)) Account {address, username, realname} = Account
    { address = _
    , username = fmap _ username
    , realname = fmap (fmap f) realname
    }

instance TraversableE Account where
instance ApplicativeE Account where

instance FunctorE Address where
  phaseMap (HeadMap f) Address {street, number, country} = Address
    { street = f street
    , number = f number
    , country = f country
    }

instance TraversableE Address where

```
