module Contract.Bridge.Storage
  ( Storage
  , mkStorage
  , HasBridgeStorage
  ) where

import Indigo

import Contract.Bridge.Types (SwapId, Swap, Outcome)

data Storage = Storage
  { swaps    :: BigMap SwapId Swap
  , outcomes :: BigMap SwapId Outcome
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

mkStorage :: Storage
mkStorage = Storage
  { swaps = mempty
  , outcomes = mempty
  }

type HasBridgeStorage s =
  ( HasField s "swaps" (BigMap SwapId Swap)
  , HasField s "outcomes" (BigMap SwapId Outcome)
  , HasStorage s
  , IsObject s
  )

instance TypeHasDoc Storage where
  typeDocMdDescription = "Managed ledger storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance HasFieldOfType Storage name field =>
         StoreHasField Storage name field where
  storeFieldOps = storeFieldOpsADT

instance HasField Storage "swaps" (BigMap SwapId Swap) where
  fieldLens = fieldLensADT #swaps

instance HasField Storage "outcomes" (BigMap SwapId Outcome) where
  fieldLens = fieldLensADT #outcomes
