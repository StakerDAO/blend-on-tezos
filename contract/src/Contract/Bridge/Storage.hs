module Contract.Bridge.Storage
  ( BridgeStorage
  , mkStorage
  , HasBridge
  ) where

import Indigo

import Contract.Bridge.Types (Outcome, Swap, SwapId)

data BridgeStorage = BridgeStorage
  { sSwaps    :: BigMap SwapId Swap
  , sOutcomes :: BigMap SwapId Outcome
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

mkStorage :: BridgeStorage
mkStorage = BridgeStorage
  { sSwaps = mempty
  , sOutcomes = mempty
  }

type HasBridge s =
  ( HasField s "swaps" (BigMap SwapId Swap)
  , HasField s "outcomes" (BigMap SwapId Outcome)
  , HasStorage s
  , IsObject s
  )

instance TypeHasDoc BridgeStorage where
  typeDocMdDescription = "Bridge storage."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "BridgeStorage", '( 'Nothing,
         '[ '("sSwaps", "Container with all swaps.")
          , '("sOutcomes", "Container with results of each swap.")
          ])
       )
    ]

instance HasFieldOfType BridgeStorage name field =>
         StoreHasField BridgeStorage name field where
  storeFieldOps = storeFieldOpsADT

instance HasField BridgeStorage "swaps" (BigMap SwapId Swap) where
  fieldLens = fieldLensADT #sSwaps

instance HasField BridgeStorage "outcomes" (BigMap SwapId Outcome) where
  fieldLens = fieldLensADT #sOutcomes
