module Data.HKD.Extensible where

-- base
import Data.Kind (Type)

infixr 9 :::

data Phases (aspects :: [Type]) where
  Nil :: Phases '[]
  (:::) :: aspect -> Phases aspects -> Phases (aspect ': aspects)

type family Head (phases :: Phases (aspect ': aspects)) :: aspect where
  Head (phase '::: _) = phase

type family Tail (phases :: Phases (aspect ': aspects)) :: Phases aspects where
  Tail (_ '::: phases) = phases

type family Interpret (aspect :: Type) (phase :: aspect) :: Type -> Type

class FunctorE (d :: Phases (aspect ': aspects) -> Type) where
  phaseMap :: PhaseMap (aspect ': aspects) p1 p2 -> d p1 -> d p2

mapHead :: (FunctorE d) => (forall x. Interpret aspect p1 x -> Interpret aspect p2 x) -> d (p1 '::: ps) -> d (p2 '::: ps)
mapHead f = phaseMap $ HeadMap f

-- StandaloneKindSignatures is only available from 8.10 onwards. Remove once we drop support for lower GHCs
#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
type PhaseMap :: forall (aspects :: [Type]) -> Phases aspects -> Phases aspects -> Type
#endif
data PhaseMap aspects (phasesIn :: Phases aspects) (phasesOut :: Phases aspects) where
  HeadMap :: (forall x. Interpret aspect pIn x -> Interpret aspect pOut x) -> PhaseMap (aspect ': aspects) (pIn '::: ps) (pOut '::: ps)
  TailMap :: PhaseMap aspects pIn pOut -> PhaseMap (aspect ': aspects) (p '::: pIn) (p '::: pOut)

#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
type PhaseTraversal :: forall (aspects :: [Type]) -> Phases aspects -> Phases aspects -> (Type -> Type) -> Type
#endif
data PhaseTraversal aspects (phasesIn :: Phases aspects) (phasesOut :: Phases aspects) f where
  HeadTraversal :: (forall x. Interpret aspect pIn x -> f (Interpret aspect pOut x)) -> PhaseTraversal (aspect ': aspects) (pIn '::: ps) (pOut '::: ps) f
  TailTraversal :: PhaseTraversal aspects pIn pOut f -> PhaseTraversal (aspect ': aspects) (p '::: pIn) (p '::: pOut) f

class (FunctorE d) => TraversableE (d :: Phases (aspect ': aspects) -> Type) where
  phaseTraversal :: forall f p1 p2. (Applicative f) => PhaseTraversal (aspect ': aspects) p1 p2 f -> d p1 -> f (d p2)

traverseHead :: (Applicative f, TraversableE d) => (forall x. Interpret aspect p1 x -> f (Interpret aspect p2 x)) -> d (p1 '::: ps) -> f (d (p2 '::: ps))
traverseHead handler = phaseTraversal $ HeadTraversal handler
