module Contract.Token.Storage
  ( Storage
  , StorageSkeleton (..)
  , LedgerValue
  , HasManagedLedgerStorage
  , mkStorage
  , mkStorageSkeleton
  ) where

import Indigo

import Lorentz.Contracts.ManagedLedger.Types (LedgerValue, StorageSkeleton (..), mkStorageSkeleton)
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface ()

data StorageFields = StorageFields
  { admin       :: Address
  , paused      :: Bool
  , totalSupply :: Natural
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

type Storage = StorageSkeleton StorageFields

type HasManagedLedgerStorage s =
  ( HasField s "admin" Address
  , HasField s "paused" Bool
  , HasField s "ledger" (BigMap Address LedgerValue)
  , HasField s "approvals" (BigMap GetAllowanceParams Natural)
  , HasField s "totalSupply" Natural
  , HasStorage s
  , IsObject s
  )

mkStorage :: Address -> Map Address Natural -> Storage
mkStorage adminAddress balances = mkStorageSkeleton balances $
  StorageFields
  { admin = adminAddress
  , paused = False
  , totalSupply = sum balances
  }

instance TypeHasDoc StorageFields where
  typeDocMdDescription = "Managed ledger storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance HasFieldOfType StorageFields name field =>
         StoreHasField StorageFields name field where
  storeFieldOps = storeFieldOpsADT

instance HasField Storage "ledger" (BigMap Address LedgerValue) where
  fieldLens = fieldLensADT #ledger

instance HasField Storage "approvals" (BigMap GetAllowanceParams Natural) where
  fieldLens = fieldLensADT #approvals

instance HasField StorageFields "admin" Address where
  fieldLens = fieldLensADT #admin

instance HasField StorageFields "paused" Bool where
  fieldLens = fieldLensADT #paused

instance HasField StorageFields "totalSupply" Natural where
  fieldLens = fieldLensADT #totalSupply

instance HasField Storage "admin" Address where
  fieldLens = fieldLensDeeper #fields

instance HasField Storage "paused" Bool where
  fieldLens = fieldLensDeeper #fields

instance HasField Storage "totalSupply" Natural where
  fieldLens = fieldLensDeeper #fields
