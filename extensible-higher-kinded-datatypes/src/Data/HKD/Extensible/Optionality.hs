module Data.HKD.Extensible.Optionality where

-- base
import Data.Functor.Identity (Identity (Identity))

-- extensible-higher-hinded-datatypes
import Data.HKD.Extensible

data Optionality = Optional | Required

type instance Interpret Optionality 'Optional = Maybe
type instance Interpret Optionality 'Required = Identity

require :: (TraversableE d) => d ('Optional '::: phases) -> Maybe (d ('Required '::: phases))
require = traverseHead $ fmap Identity
