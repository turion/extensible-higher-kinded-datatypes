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

traverseHead_ :: forall f aspect p1 ps d. (Applicative f, TraversableE d) => (forall x. Interpret aspect p1 x -> f (Interpret aspect p1 x)) -> d (p1 '::: ps) -> f ()
traverseHead_ handler = fmap unit . traverseHead handler
  where
    unit :: d (p1 '::: ps) -> ()
    unit _ = ()

#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
type PhaseZip :: forall (aspects :: [Type]) -> Phases aspects -> Phases aspects -> Phases aspects -> Type
#endif
data PhaseZip aspects (phases1 :: Phases aspects) (phases2 :: Phases aspects) (phases3 :: Phases aspects) where
  HeadZip :: (forall x . Interpret aspect p1 x -> Interpret aspect p2 x -> Interpret aspect p3 x) -> PhaseZip (aspect ': aspects) (p1 '::: ps) (p2 '::: ps) (p3 '::: ps)
  TailZip :: PhaseZip aspects phases1 phases2 phases3 -> PhaseZip (aspect ': aspects) (p '::: phases1) (p '::: phases2) (p '::: phases3)

class (FunctorE d) => ApplicativeE (d :: Phases (aspect ': aspects) -> Type) where
  phaseZip :: PhaseZip (aspect ': aspects) phases1 phases2 phases3 -> d phases1 -> d phases2 -> d phases3

zipWithHead :: ApplicativeE d => (forall x . Interpret aspect p1 x -> Interpret aspect p2 x -> Interpret aspect p3 x) -> d (p1 '::: phases) -> d (p2 '::: phases) -> d (p3 '::: phases)
zipWithHead f = phaseZip $ HeadZip f
