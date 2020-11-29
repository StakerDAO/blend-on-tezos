module Contract.Token.Impl
  ( entrypoints
  , Parameter (..)
  ) where

import Indigo

import qualified Indigo.Contracts.ManagedLedger as ML
import qualified Lorentz.Contracts.ManagedLedger.Doc as L
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (ApproveParams, GetAllowanceArg,
                                                         GetAllowanceParams, GetBalanceArg,
                                                         GetTotalSupplyArg, TransferParams)
import Lorentz.Contracts.Spec.ManagedLedgerInterface (ApproveCasParams, BurnParams, MintParams)

import Contract.Token.Storage (HasManagedLedgerStorage)

data Parameter
  = Transfer         TransferParams
  | Approve          ApproveParams
  | ApproveCAS       ApproveCasParams
  | GetAllowance     GetAllowanceArg
  | GetBalance       GetBalanceArg
  | GetTotalSupply   GetTotalSupplyArg
  | SetPause         Bool
  | SetAdministrator Address
  | GetAdministrator (View () Address)
  | Mint             MintParams
  | Burn             BurnParams
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

entrypoints
  :: forall storage param.
     ( param :~> Parameter
     , HasManagedLedgerStorage storage
     , HasSideEffects
     )
  => IndigoEntrypoint param
entrypoints param = do
  entryCaseSimple param
    ( #cTransfer #= ML.transfer @storage
    , #cApprove #= ML.approve @storage
    , #cApproveCAS #= approveCAS @storage
    , #cGetAllowance #= ML.getAllowance @storage
    , #cGetBalance #= ML.getBalance @storage
    , #cGetTotalSupply #= ML.getTotalSupply @storage
    , #cSetPause #= ML.setPause @storage
    , #cSetAdministrator #= setAdministrator @storage
    , #cGetAdministrator #= getAdministrator @storage
    , #cMint #= ML.mint @storage
    , #cBurn #= ML.burn @storage
    )

-- | Compares the expected allowance value with the actual one
-- and sets a new one if they match.
approveCAS
  :: forall s ap.
     ( ap :~> ApproveCasParams
     , HasManagedLedgerStorage s
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
     , HasManagedLedgerStorage s
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
     , HasManagedLedgerStorage s
     , HasSideEffects
     )
  => IndigoEntrypoint gp
getAdministrator parameter = do
  doc $ DDescription L.getAdministratorDoc
  project parameter $ \_ -> getStorageField @s #admin

----------------------------------------------------------------------------
--  Helpers
----------------------------------------------------------------------------

-- | Return current allowance for spender.
allowance
  :: forall s ap.
     ( ap :~> GetAllowanceParams
     , HasManagedLedgerStorage s
     )
  => ap -> IndigoFunction Natural
allowance parameter = do
  approvals <- getStorageField @s #approvals
  ifSome (approvals #: parameter) return (return $ 0 nat)

-- | Set allowance for spender.
setAllowance
  :: forall s owner spender value.
     ( spender :~> Address, owner :~> Address , value :~> Natural
     , HasManagedLedgerStorage s
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
