module Types
  ( Storage
  , StorageSkeleton (..)
  , LedgerValue
  , mkStorage
  , mkStorageSkeleton
  ) where

import Prelude (sum)

import Indigo as I (HasField (..), fieldLensADT, fieldLensDeeper)
import Lorentz
import Lorentz.Contracts.ManagedLedger.Types (LedgerValue, StorageSkeleton (..), mkStorageSkeleton)
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)

----------------------------------------------------------------------------
-- Storage
----------------------------------------------------------------------------
type SwapId = ByteString

data Swap = MkSwap
   { from        :: Address
   , to          :: Address
   , amount      :: Natural
   , releaseTime :: Timestamp
   } deriving stock Generic
     deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Swap where
  typeDocMdDescription = "Swap storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data Outcome
   = Refunded
   | HashRevealed ByteString
   | SecretRevealed ByteString
   deriving stock Generic
   deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Outcome where
  typeDocMdDescription = "Outcome storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  , swaps       :: BigMap SwapId Swap
  , outcomes    :: BigMap SwapId Outcome
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc StorageFields where
  typeDocMdDescription = "Managed ledger storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance HasFieldOfType StorageFields name field =>
         StoreHasField StorageFields name field where
  storeFieldOps = storeFieldOpsADT

type Storage = StorageSkeleton StorageFields

----------------------------------------------------------------------------
-- HasField instances for Indigo version
----------------------------------------------------------------------------

instance I.HasField Storage "ledger" (BigMap Address LedgerValue) where
  fieldLens = fieldLensADT #ledger

instance I.HasField Storage "approvals" (BigMap GetAllowanceParams Natural) where
  fieldLens = fieldLensADT #approvals

instance I.HasField StorageFields "admin" Address where
  fieldLens = fieldLensADT #admin

instance I.HasField StorageFields "paused" Bool where
  fieldLens = fieldLensADT #paused

instance I.HasField StorageFields "totalSupply" Natural where
  fieldLens = fieldLensADT #totalSupply

instance I.HasField Storage "admin" Address where
  fieldLens = fieldLensDeeper #fields

instance I.HasField Storage "paused" Bool where
  fieldLens = fieldLensDeeper #fields

instance I.HasField Storage "totalSupply" Natural where
  fieldLens = fieldLensDeeper #fields

-- | Create a default storage with ability to set some balances to
-- non-zero values.
mkStorage :: Address -> Map Address Natural -> Storage
mkStorage adminAddress balances = mkStorageSkeleton balances $
  StorageFields
  { admin = adminAddress
  , paused = False
  , totalSupply = sum balances
  , swaps = mempty
  , outcomes = mempty
  }
