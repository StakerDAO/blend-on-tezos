{-# LANGUAGE BlockArguments #-}

module Contract
  ( -- * Storage
    Storage
  , mkStorage
  , LedgerValue

  , printContractWithInitStorage
  , managedLedgerIndigo
  ) where

import Indigo

import qualified Indigo.Contracts.ManagedLedger as ML
import qualified Lorentz.Contracts.ManagedLedger.Doc as L
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Lorentz.Contracts.Spec.ManagedLedgerInterface (ApproveCasParams, BurnParams, MintParams)
import Tezos.Address (unsafeParseAddress)
import Types
import Universum (writeFile)

type IStorageC s =
  ( ILedgerC s
  , HasField s "admin" Address
  , HasField s "paused" Bool
  , HasField s "text" MText
  )

type ILedgerC s =
  ( HasField s "ledger" (BigMap Address LedgerValue)
  , HasField s "approvals" (BigMap GetAllowanceParams Natural)
  , HasField s "totalSupply" Natural
  , HasStorage s
  , IsObject s
  )

----------------------------------------------------------------------------
-- Entrypoints
----------------------------------------------------------------------------

-- | Compares the expected allowance value with the actual one
-- and sets a new one if they match.
approveCAS
  :: forall s ap.
     ( ap :~> ApproveCasParams
     , ILedgerC s, HasField s "paused" Bool
     )
  => IndigoEntrypoint ap
approveCAS parameter = do
  doc $ DDescription L.approveCASDoc

  ML.ensureNotPaused @s

  spender <- new$ parameter #! #spender
  value <- new$ parameter #! #value
  expectedAllowance <- new$ parameter #! #expected

  actualAllowance <- allowance @s $ construct
    ( sender !~ #owner
    , spender !~ #spender
    )

  allowanceMismatchError <- new$ pair
    (actualAllowance !~ #actual)
    (expectedAllowance !~ #expected)

  when (expectedAllowance /= actualAllowance) $
    failCustom #allowanceMismatch allowanceMismatchError

  setAllowance @s sender spender value

-- | Changes current administrator.
setAdministrator
  :: forall s sp.
     ( sp :~> Address
     , IStorageC s
     )
  => IndigoEntrypoint sp
setAdministrator parameter = do
  doc $ DDescription L.setAdministratorDoc
  authorizeAdmin @s
  setStorageField @s #admin parameter

-- | Returns current administrator address.
getAdministrator
  :: forall s gp.
     ( gp :~> View () Address
     , IStorageC s
     , HasSideEffects
     )
  => IndigoEntrypoint gp
getAdministrator parameter = do
  doc $ DDescription L.getAdministratorDoc
  project parameter $ \_ -> getStorageField @s #admin

setText
  :: forall s bp.
     ( bp :~> MText
     , IStorageC s
     )
  => IndigoEntrypoint bp
setText parameter = do
  setStorageField @s #text parameter

----------------------------------------------------------------------------
--  Helpers
----------------------------------------------------------------------------

-- | Return current allowance for spender.
allowance
  :: forall s ap.
     ( ap :~> GetAllowanceParams
     , ILedgerC s
     )
  => ap -> IndigoFunction Natural
allowance parameter = do
  approvals <- getStorageField @s #approvals
  ifSome (approvals #: parameter) return (return $ 0 nat)

-- | Set allowance for spender.
setAllowance
  :: forall s owner spender value.
     ( spender :~> Address, owner :~> Address , value :~> Natural
     , ILedgerC s
     )
  => owner -> spender -> value -> IndigoProcedure
setAllowance owner spender value = do
  approvals <- getStorageField @s #approvals
  gap <- new$ construct
    ( owner   !~ #owner
    , spender !~ #spender
    )

  setStorageField @s #approvals $ approvals !: (gap, nonZero value)

-- | Ensure that sender is admin.
authorizeAdmin
  :: forall s. (HasStorage s, HasField s "admin" Address)
  => IndigoProcedure
authorizeAdmin = do
  doc $ L.DRequireRole "administrator"
  currentAdmin <- getStorageField @s #admin
  when (sender /= currentAdmin) $
    failCustom_ #senderIsNotAdmin

----------------------------------------------------------------------------
-- Contract
----------------------------------------------------------------------------
data Parameter
  = Transfer         AL.TransferParams
  | Approve          AL.ApproveParams
  | ApproveCAS       ApproveCasParams
  | GetAllowance     AL.GetAllowanceArg
  | GetBalance       AL.GetBalanceArg
  | GetTotalSupply   AL.GetTotalSupplyArg
  | SetPause         Bool
  | SetAdministrator Address
  | GetAdministrator (View () Address)
  | Mint             MintParams
  | Burn             BurnParams
  | SetText          MText
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

managedLedgerIndigo :: IndigoContract Parameter Storage
managedLedgerIndigo param = contractName "Managed Ledger" do
  contractGeneralDefault
  docStorage @Storage
  doc $ DDescription L.contractDoc
  entryCaseSimple param
    ( #cTransfer //-> ML.transfer @Storage
    , #cApprove //-> ML.approve @Storage
    , #cApproveCAS //-> approveCAS @Storage
    , #cGetAllowance //-> ML.getAllowance @Storage
    , #cGetBalance //-> ML.getBalance @Storage
    , #cGetTotalSupply //-> ML.getTotalSupply @Storage
    , #cSetPause //-> ML.setPause @Storage
    , #cSetAdministrator //-> setAdministrator @Storage
    , #cGetAdministrator //-> getAdministrator @Storage
    , #cMint //-> ML.mint @Storage
    , #cBurn //-> ML.burn @Storage
    , #cSetText //-> setText @Storage
    )

printContractWithInitStorage :: IO ()
printContractWithInitStorage = do
  let st = toStrict $ printLorentzValue True $ mkStorage (unsafeParseAddress "tz1b3eGoYhuJ6cPPHAr1SZrYdZGtoYGyxqEW") mempty
  saveAsMichelson @Parameter @Storage managedLedgerIndigo "morley-contract/contract.tz"
  writeFile "morley-contract/storage.tz" st
