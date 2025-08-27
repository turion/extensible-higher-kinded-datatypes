module Data.HKD.Extensible.Label where

import Data.Functor.Const (Const (..))
import Data.HKD.Extensible
import Data.Kind (Type)

data Label (labelType :: Type) = Label

type instance Interpret (Label labelType) 'Label = Const labelType

listLabels :: (TraversableE d) => d (('Label :: Label t) '::: phases) -> [t]
listLabels = fst . traverseHead_ (\c@(Const label) -> ([label], c))
