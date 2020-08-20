{-# LANGUAGE BlockArguments #-}

module Contract
  ( Storage
  , mkStorage
  , LedgerValue
  , printContractWithInitStorage
  , managedLedgerIndigo
  ) where

import Indigo

import Fmt (Buildable (..), (+|), (|+))
import qualified Indigo.Contracts.ManagedLedger as ML
import Lorentz (cCode)
import qualified Lorentz.Contracts.ManagedLedger.Doc as L
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)
import qualified Lorentz.Contracts.Spec.ApprovableLedgerInterface as AL
import Lorentz.Contracts.Spec.ManagedLedgerInterface (ApproveCasParams, BurnParams, MintParams)
import Tezos.Address (unsafeParseAddress)
import Universum (writeFile)

import Data.Coerce (coerce)
import Types (LedgerValue, Outcome, OutcomeStatus (..), Storage, Swap, SwapId (..), mkStorage)

type IStorageC s =
  ( HasField s "admin" Address
  , HasField s "paused" Bool
  , HasField s "swaps" (BigMap SwapId Swap)
  , HasField s "outcomes" (BigMap SwapId Outcome)
  , HasField s "ledger" (BigMap Address LedgerValue)
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
     , IStorageC s, HasField s "paused" Bool
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

type instance ErrorArg "swapLockAlreadyExists" = SwapId

instance Buildable (CustomError "swapLockAlreadyExists") where
  build (CustomError _ swapId) =
    "Swap lock with " +| decodeUtf8 @Text @ByteString (coerce swapId) |+ " id already exists"

instance CustomErrorHasDoc "swapLockAlreadyExists" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Lock with this id already exists"
  customErrArgumentSemantics = Just "swap id"

lock
  :: forall s sp.
     ( sp :~> LockParams
     , IStorageC s
     )
  => IndigoEntrypoint sp
lock parameter = do
  swaps <- getStorageField @s #swaps
  swapId <- new$ parameter #! #id
  whenSome (swaps #: swapId) $ \_ -> failCustom #swapLockAlreadyExists swapId
  
  debitFrom @s sender $ parameter #! #amount
  
  outcomes <- getStorageField @s #outcomes
  setStorageField @s #swaps $ swaps +: (swapId, construct
                                                      ( sender
                                                      , parameter #! #to
                                                      , parameter #! #amount
                                                      , parameter #! #releaseTime
                                                      ))
  whenSome (parameter #! #secretHash) $ \hash -> setStorageField @s #outcomes $ outcomes +: (swapId, construct (constExpr HashRevealed, some (varExpr hash), constExpr Nothing))
  pure ()

----------------------------------------------------------------------------
--  Helpers
----------------------------------------------------------------------------

-- | Debit the given amount of tokens from a given address in ledger.
debitFrom
  :: forall s from value.
     ( from :~> Address, value :~> Natural
     , IStorageC s
     )
  => from -> value -> IndigoProcedure
debitFrom from val = do
  -- Balance check and the corresponding update.
  ledger_ <- getStorageField @s #ledger

  ifSome (ledger_ #: from)
    (\ledgerValue -> do
        oldBalance <- new$ ledgerValue #~ #balance
        newBalance <- ifSome (isNat $ oldBalance - val)
          return (failNotEnoughBalance @Natural oldBalance)
        newLedgerValue <- nonEmptyLedgerValue (newBalance !~ #balance)
        setStorageField @s #ledger $ ledger_ !: (from, newLedgerValue)
    )
    (failNotEnoughBalance @() (0 nat))
    where
      failNotEnoughBalance :: forall r ex. (ex :~> Natural) => ex -> IndigoM r
      failNotEnoughBalance curBalance = failCustom @r #notEnoughBalance $ pair
        (val !~ #required)
        (curBalance !~ #present)

-- | Ensure that given 'LedgerValue' value cannot be safely removed
-- and return it.
nonEmptyLedgerValue
  :: ( ledgerValue :~> LedgerValue )
  => ledgerValue -> IndigoFunction (Maybe LedgerValue)
nonEmptyLedgerValue ledgerValue =
  if ledgerValue #~ #balance == 0 nat
    then return none
    else return (some ledgerValue)

-- | Return current allowance for spender.
allowance
  :: forall s ap.
     ( ap :~> GetAllowanceParams
     , IStorageC s
     )
  => ap -> IndigoFunction Natural
allowance parameter = do
  approvals <- getStorageField @s #approvals
  ifSome (approvals #: parameter) return (return $ 0 nat)

-- | Set allowance for spender.
setAllowance
  :: forall s owner spender value.
     ( spender :~> Address, owner :~> Address , value :~> Natural
     , IStorageC s
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

data LockParams = LockParams
  { id :: SwapId
  , to :: Address
  , amount :: Natural
  , releaseTime :: Timestamp
  , secretHash :: Maybe ByteString
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)
    
instance TypeHasDoc LockParams where
  typeDocMdDescription = "Outcome storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

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
  | Lock             LockParams
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

managedLedgerIndigo :: IndigoContract Parameter Storage
managedLedgerIndigo param = contractName "BLND on Tezos" do
  contractGeneralDefault
  docStorage @Storage
  doc $ DDescription
    "This documentation describes a smart contract which implements FA1.2 interface and coin swap."
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
    , #cLock //-> lock @Storage
    )

printContractWithInitStorage :: IO ()
printContractWithInitStorage = do
  let storage = toStrict $
        printLorentzValue True $
        mkStorage (unsafeParseAddress "tz1b3eGoYhuJ6cPPHAr1SZrYdZGtoYGyxqEW") mempty
      documentation = toStrict $
        contractDocToMarkdown $ buildLorentzDocWithGitRev DGitRevisionUnknown $
        cCode $ defaultContract $ compileIndigoContract @Parameter @Storage managedLedgerIndigo
  saveAsMichelson @Parameter @Storage managedLedgerIndigo "morley-contract/contract.tz"
  writeFile "morley-contract/storage.tz" storage
  writeFile "doc/specification.md" documentation
