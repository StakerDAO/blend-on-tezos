module Contract.Bridge.Impl
  ( entrypoints
  , Parameter (..)
  ) where

import Indigo

import Contract.Bridge.Errors ()
import Contract.Bridge.Storage (HasBridge)
import Contract.Bridge.Types (ClaimRefundParams, GetOutcomeParams, GetSwapParams, LockParams,
                              Outcome (..), RedeemParams, RevealSecretHashParams)
import Contract.Token.Storage (HasManagedLedgerStorage, LedgerValue)

data Parameter
  = Lock LockParams
  | RevealSecretHash RevealSecretHashParams
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
    ( #cLock  //-> lock @storage
    , #cRevealSecretHash //-> revealSecretHash @storage
    , #cRedeem //-> redeem @storage
    , #cClaimRefund //-> claimRefund @storage
    , #cGetSwap //-> getSwap @storage
    , #cGetOutcome //-> getOutcome @storage
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

  whenSome (parameter #! #lpSecretHash) $ \hash ->
    setStorageField @s #outcomes $ outcomes +: (swapId, wrap #cHashRevealed hash)

revealSecretHash
  :: forall s sp.
     ( sp :~> RevealSecretHashParams
     , HasBridge s
     )
  => IndigoEntrypoint sp
revealSecretHash parameter = do
  swaps <- getStorageField @s #swaps
  swapId <- new$ parameter #! #rshpId

  ifNone (swaps #: swapId)
    (void $ failCustom #swapLockDoesNotExist swapId) $
    \s -> when (sender /= (s #! #sFrom)) $ failCustom_ #senderIsNotTheInitiator

  outcomes <- getStorageField @s #outcomes
  whenSome (outcomes #: swapId) $ \_ -> failCustom #secretHashIsAlreadySet swapId

  setStorageField @s #outcomes $ outcomes +:
    (swapId, wrap #cHashRevealed $ parameter #! #rshpSecretHash)

redeem
  :: forall s sp.
     ( sp :~> RedeemParams
     , HasBridge s
     , HasManagedLedgerStorage s
     )
  => IndigoEntrypoint sp
redeem parameter = do
  swapId <- new$ parameter #! #rpId

  secret <- new$ parameter #! #rpSecret
  maxSecretLength <- new$ 32 nat -- TODO now const but maybe make it configurable
  when (size secret > maxSecretLength) $
    failCustom #tooLongSecret $ construct (varExpr maxSecretLength, size secret)

  outcomes <- getStorageField @s #outcomes
  ifSome (outcomes #: swapId)
    (\o -> case_ o
      ( #cRefunded //->
          \_ -> void $ failCustom #wrongOutcomeStatus [mt|Refunded|]
      , #cHashRevealed //->
          \secretHash -> when (sha256 secret /= secretHash) $ failCustom_ #invalidSecret
      , #cSecretRevealed //->
          \_ -> void $ failCustom #wrongOutcomeStatus [mt|SecretRevealed|]
      )
    )
    (void $ failCustom #swapLockDoesNotExist swapId)

  swaps <- getStorageField @s #swaps

  ifSome (swaps #: swapId)
    (\s -> do
       when (now >= s #! #sReleaseTime) $ failCustom #swapIsOver $ s #! #sReleaseTime
       setStorageField @s #outcomes $ outcomes +: (swapId, wrap #cSecretRevealed secret)
       creditTo @s (s #! #sTo) (s #! #sAmount)
    )
    (void $ failCustom #swapLockDoesNotExist swapId)

claimRefund
  :: forall s sp.
     ( sp :~> ClaimRefundParams
     , HasBridge s
     , HasManagedLedgerStorage s
     )
  => IndigoEntrypoint sp
claimRefund parameter = do
  swapId <- new$ parameter #! #crpId
  outcomes <- getStorageField @s #outcomes

  ifSome (outcomes #: swapId)
    (\o -> case_ o
      ( #cRefunded //-> \_ -> void $ failCustom #wrongOutcomeStatus [mt|Refunded|]
      , #cHashRevealed //-> \_ -> pass
      , #cSecretRevealed //-> \_ -> void $ failCustom #wrongOutcomeStatus [mt|SecretRevealed|]
      )
    )
    $ pass

  swaps <- getStorageField @s #swaps

  ifSome (swaps #: swapId)
    (\s -> do
       when (now < s #! #sReleaseTime) $ failCustom #fundsLock $ s #! #sReleaseTime
       setStorageField @s #outcomes $ outcomes +: (swapId, Refunded ())
       creditTo @s (s #! #sFrom) (s #! #sAmount)
    )
    (void $ failCustom #swapLockDoesNotExist swapId)

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
