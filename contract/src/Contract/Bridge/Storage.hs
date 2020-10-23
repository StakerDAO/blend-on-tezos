module Contract.Bridge.Storage
  ( BridgeStorage (..)
  , mkStorage
  , HasBridge
  ) where

import Indigo

import Contract.Bridge.Types (Outcome, SecretHash, Swap)

data BridgeStorage = BridgeStorage
  { sSwaps    :: BigMap SecretHash Swap
  , sOutcomes :: BigMap SecretHash Outcome
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue, HasAnnotation)

mkStorage :: BridgeStorage
mkStorage = BridgeStorage
  { sSwaps = mempty
  , sOutcomes = mempty
  }

type HasBridge s =
  ( HasField s "swaps" (BigMap SecretHash Swap)
  , HasField s "outcomes" (BigMap SecretHash Outcome)
  , HasStorage s
  , IsObject s
  )

instance TypeHasDoc BridgeStorage where
  typeDocMdDescription = "Bridge storage."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "BridgeStorage", '( 'Nothing,
         '[ '("sSwaps", "Container with all current swaps.")
          , '("sOutcomes", "Container with received secrets of each swap.")
          ])
       )
    ]

instance HasFieldOfType BridgeStorage name field =>
         StoreHasField BridgeStorage name field where
  storeFieldOps = storeFieldOpsADT

instance HasField BridgeStorage "swaps" (BigMap SecretHash Swap) where
  fieldLens = fieldLensADT #sSwaps

instance HasField BridgeStorage "outcomes" (BigMap SecretHash Outcome) where
  fieldLens = fieldLensADT #sOutcomes
