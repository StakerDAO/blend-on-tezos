module Contract.Bridge.Impl
  ( entrypoints
  , Parameter
  ) where

import Indigo

import qualified Indigo.Contracts.ManagedLedger as ML
import Lorentz.Contracts.Spec.ManagedLedgerInterface (BurnParams)

import Contract.Bridge.Errors ()
import Contract.Bridge.Storage (HasBridgeStorage)
import Contract.Bridge.Types (LockParams, OutcomeStatus (..))
import Contract.Token.Storage (HasManagedLedgerStorage, LedgerValue)

data Parameter
  = Burnq BurnParams
  | Lock  LockParams
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

entrypoints
  :: forall storage param.
     ( param :~> Parameter
     , HasManagedLedgerStorage storage
     , HasBridgeStorage storage
     , HasSideEffects
     )
  => IndigoEntrypoint param
entrypoints param = do
  entryCaseSimple param
    ( #cBurnq //-> ML.burn @storage
    , #cLock  //-> lock @storage
    )

lock
  :: forall s sp.
     ( sp :~> LockParams
     , HasManagedLedgerStorage s
     , HasBridgeStorage s
     )
  => IndigoEntrypoint sp
lock parameter = do
  swaps <- getStorageField @s #swaps
  swapId <- new$ parameter #! #lpId
  whenSome (swaps #: swapId) $ \_ -> failCustom #swapLockAlreadyExists swapId

  debitFrom @s sender $ parameter #! #lpAmount

  outcomes <- getStorageField @s #outcomes
  setStorageField @s #swaps $ swaps +: (swapId, construct
                                                      ( sender
                                                      , parameter #! #lpTo
                                                      , parameter #! #lpAmount
                                                      , parameter #! #lpReleaseTime
                                                      ))
  whenSome (parameter #! #lpSecretHash) $ \hash -> setStorageField @s #outcomes $ outcomes +: (swapId, construct (constExpr HashRevealed, some (varExpr hash), constExpr Nothing))
  pure ()

----------------------------------------------------------------------------
--  Helpers
----------------------------------------------------------------------------

-- | Debit the given amount of tokens from a given address in ledger.
debitFrom
  :: forall s from value.
     ( from :~> Address, value :~> Natural
     , HasManagedLedgerStorage s
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
