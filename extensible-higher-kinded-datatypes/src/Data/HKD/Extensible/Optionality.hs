module Data.HKD.Extensible.Optionality
  (module Data.HKD.Extensible.Optionality,
  module X) where

-- base
import Data.Functor.Identity as X (Identity (Identity))

-- extensible-higher-hinded-datatypes
import Data.HKD.Extensible

data Optionality = Optional | Required

type instance Interpret Optionality 'Optional = Maybe
type instance Interpret Optionality 'Required = Identity

requireP :: PhaseTraversal (Optionality ': aspects) ('Optional '::: phases) ('Required '::: phases) Maybe
requireP = HeadTraversal$ fmap Identity

require :: (TraversableE d) => d ('Optional '::: phases) -> Maybe (d ('Required '::: phases))
require = traverseHead $ fmap Identity
