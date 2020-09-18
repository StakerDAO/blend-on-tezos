module Contract.Token.Storage
  ( ManagedLedgerStorage
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
import Util.Named ((.!))

data ManagedLedgerStorage = ManagedLedgerStorage
  { mlsLedger      :: BigMap Address LedgerValue
  , mlsApprovals   :: BigMap GetAllowanceParams Natural
  , mlsAdmin       :: Address
  , mlsPaused      :: Bool
  , mlsTotalSupply :: Natural
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

type HasManagedLedgerStorage s =
  ( HasField s "admin" Address
  , HasField s "paused" Bool
  , HasField s "ledger" (BigMap Address LedgerValue)
  , HasField s "approvals" (BigMap GetAllowanceParams Natural)
  , HasField s "totalSupply" Natural
  , HasStorage s
  , IsObject s
  )

mkStorage :: Address -> Map Address Natural -> ManagedLedgerStorage
mkStorage adminAddress balances = ManagedLedgerStorage
  { mlsLedger      = BigMap $ (#balance .!) <$> balances
  , mlsApprovals   = mempty
  , mlsAdmin       = adminAddress
  , mlsPaused      = False
  , mlsTotalSupply = sum balances
  }

instance TypeHasDoc ManagedLedgerStorage where
  typeDocMdDescription = "Managed ledger storage."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "ManagedLedgerStorage", '( 'Nothing,
         '[ '("mlsLedger", "Ledger which map addresses to its token balance.")
          , '("mlsApprovals", "Map from approvals to its value.")
          , '("mlsAdmin", "Addres of the admin of the token.")
          , '("mlsPaused", "Pause of managed ledger.")
          , '("mlsTotalSupply", "Total amount of tokens.")
          ])
       )
    ]

instance HasFieldOfType ManagedLedgerStorage name field =>
         StoreHasField ManagedLedgerStorage name field where
  storeFieldOps = storeFieldOpsADT

instance HasField ManagedLedgerStorage "ledger" (BigMap Address LedgerValue) where
  fieldLens = fieldLensADT #mlsLedger

instance HasField ManagedLedgerStorage "approvals" (BigMap GetAllowanceParams Natural) where
  fieldLens = fieldLensADT #mlsApprovals

instance HasField ManagedLedgerStorage "admin" Address where
  fieldLens = fieldLensADT #mlsAdmin

instance HasField ManagedLedgerStorage "paused" Bool where
  fieldLens = fieldLensADT #mlsPaused

instance HasField ManagedLedgerStorage "totalSupply" Natural where
  fieldLens = fieldLensADT #mlsTotalSupply
