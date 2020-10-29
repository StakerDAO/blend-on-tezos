module Contract.Bridge.Impl
  ( entrypoints
  , Parameter (..)
  ) where

import Indigo

import Contract.Bridge.Errors ()
import Contract.Bridge.Storage (HasBridge)
import Contract.Bridge.Types (ClaimRefundParams, ConfirmSwapParams, GetOutcomeParams, GetSwapParams,
                              LockParams, RedeemParams)
import Contract.Token.Storage (HasManagedLedgerStorage, LedgerValue)

data Parameter
  = Lock LockParams
  | ConfirmSwap ConfirmSwapParams
  | Redeem RedeemParams
  | ClaimRefund ClaimRefundParams
  | GetSwap GetSwapParams
  | GetOutcome GetOutcomeParams
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdPlain

entrypoints
  :: forall storage param.
     ( param :~> Parameter
     , HasManagedLedgerStorage storage
     , HasBridge storage
     , HasSideEffects
     )
  => IndigoEntrypoint param
entrypoints param = do
  entryCaseSimple param
    ( #cLock  #= lock @storage
    , #cConfirmSwap #= confirmSwap @storage
    , #cRedeem #= redeem @storage
    , #cClaimRefund #= claimRefund @storage
    , #cGetSwap #= getSwap @storage
    , #cGetOutcome #= getOutcome @storage
    )

lock
  :: forall s sp.
     ( sp :~> LockParams
     , HasManagedLedgerStorage s
     , HasBridge s
     )
  => IndigoEntrypoint sp
lock parameter = do
  swaps <- getStorageField @s #swaps
  secretHash <- new$ parameter #! #lpSecretHash
  whenSome (swaps #: secretHash) $ \_ -> failCustom #swapLockAlreadyExists secretHash

  debitFrom @s sender $ parameter #! #lpAmount + parameter #! #lpFee

  setStorageField @s #swaps $ swaps +: (secretHash, construct
    ( sender
    , parameter #! #lpTo
    , parameter #! #lpAmount
    , parameter #! #lpReleaseTime
    , parameter #! #lpFee
    , parameter #! #lpConfirmed
    ))

confirmSwap
  :: forall s sp.
     ( sp :~> ConfirmSwapParams
     , HasBridge s
     )
  => IndigoEntrypoint sp
confirmSwap parameter = do
  swaps <- getStorageField @s #swaps
  secretHash <- new$ parameter #! #cspSecretHash

  ifNone (swaps #: secretHash)
    (void $ failCustom #swapLockDoesNotExist secretHash) $
    \s -> do
      when (sender /= s #! #sFrom) $ failCustom_ #senderIsNotTheInitiator
      when (s #! #sConfirmed == constExpr True) $ failCustom #swapIsAlreadyConfirmed secretHash
      s =: s !! (#sConfirmed, constExpr True)
      setStorageField @s #swaps $ swaps !: (secretHash, some s)

redeem
  :: forall s sp.
     ( sp :~> RedeemParams
     , HasBridge s
     , HasManagedLedgerStorage s
     )
  => IndigoEntrypoint sp
redeem parameter = do
  secret <- new$ parameter #! #rpSecret
  secretHash <- new$ construct $ blake2b secret

  maxSecretLength <- new$ 32 nat -- TODO now const but maybe make it configurable
  when (size secret > maxSecretLength) $
    failCustom #tooLongSecret $ construct (varExpr maxSecretLength, size secret)

  outcomes <- getStorageField @s #outcomes
  swaps <- getStorageField @s #swaps

  whenSome (outcomes #: secretHash) $ \_ -> failCustom #swapIsOver secretHash

  ifSome (swaps #: secretHash)
    (\s -> do
       unless (s #! #sConfirmed) $ failCustom #swapIsNotConfirmed secretHash

       creditTo @s (s #! #sTo) (s #! #sAmount + s #! #sFee)

       setStorageField @s #outcomes $ outcomes +: (secretHash, construct $ varExpr secret)
       setStorageField @s #swaps $ swaps -: secretHash
    )
    (void $ failCustom #swapLockDoesNotExist secretHash)

claimRefund
  :: forall s sp.
     ( sp :~> ClaimRefundParams
     , HasBridge s
     , HasManagedLedgerStorage s
     )
  => IndigoEntrypoint sp
claimRefund parameter = do
  secretHash <- new$ parameter #! #crpSecretHash
  swaps <- getStorageField @s #swaps
  ifSome (swaps #: secretHash)
    (\s -> do
       when (now < s #! #sReleaseTime) $ failCustom #fundsLock $ s #! #sReleaseTime

       when (s #! #sFee /= 0 nat) $ creditTo @s (s #! #sTo) $ s #! #sFee
       creditTo @s (s #! #sFrom) (s #! #sAmount)

       setStorageField @s #swaps $ swaps -: secretHash
    )
    (void $ failCustom #swapLockDoesNotExist secretHash)

getSwap
  :: forall s sp.
     ( sp :~> GetSwapParams
     , HasBridge s
     , HasSideEffects
     )
  => IndigoEntrypoint sp
getSwap parameter = do
  swaps <- getStorageField @s #swaps
  project parameter $ \p -> return $ swaps #: p

getOutcome
  :: forall s sp.
     ( sp :~> GetOutcomeParams
     , HasBridge s
     , HasSideEffects
     )
  => IndigoEntrypoint sp
getOutcome parameter = do
  outcomes <- getStorageField @s #outcomes
  project parameter $ \p -> return $ outcomes #: p

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

-- | Credit the given amount of tokens to a given address in ledger.
creditTo
  :: forall s to value.
     ( to :~> Address, value :~> Natural
     , HasManagedLedgerStorage s
     )
  => to -> value -> IndigoProcedure
creditTo to value = do
  ledger_ <- getStorageField @s #ledger

  when (value /= 0 nat) do
    newBalance <- ifSome (ledger_ #: to)
      (\ledgerValue -> return $ (value + ledgerValue #~ #balance) !~ #balance)
      (return $ value !~ #balance)

    setStorageField @s #ledger $ ledger_ +: (to, newBalance)

-- | Ensure that given 'LedgerValue' value cannot be safely removed
-- and return it.
nonEmptyLedgerValue
  :: ( ledgerValue :~> LedgerValue )
  => ledgerValue -> IndigoFunction (Maybe LedgerValue)
nonEmptyLedgerValue ledgerValue =
  if ledgerValue #~ #balance == 0 nat
    then return none
    else return (some ledgerValue)
