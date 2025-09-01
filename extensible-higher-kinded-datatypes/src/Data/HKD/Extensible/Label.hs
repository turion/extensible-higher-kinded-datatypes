module Data.HKD.Extensible.Label
  (
    module Data.HKD.Extensible.Label,
    module X) where

-- base
import Data.Functor.Const as X (Const (..))
import Data.Kind (Type)
import Data.Functor.Identity as X (Identity)

-- extensible-higher-kinded-datatypes
import Data.HKD.Extensible

data Label (labelType :: Type) = Value | Label

type instance Interpret (Label labelType) 'Value = Identity
type instance Interpret (Label labelType) 'Label = Const labelType

listLabels :: (TraversableE d) => d (('Label :: Label t) '::: phases) -> [t]
listLabels = fst . traverseHead_ (\c@(Const label) -> ([label], c))
