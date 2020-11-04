module Contract.Bridge.Impl
  ( entrypoints
  , Parameter (..)
  ) where

import Indigo

import qualified Indigo.Contracts.ManagedLedger as ML

import Contract.Bridge.Errors ()
import Contract.Bridge.Storage (HasBridge)
import Contract.Bridge.Types (ClaimRefundParams, ConfirmSwapParams, GetOutcomeParams, GetSwapParams,
                              LockParams, RedeemParams, SwapTransferOperation (..))
import Contract.Token.Storage (HasManagedLedgerStorage)

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
  ML.ensureNotPaused @s

  swaps <- getStorageField @s #swaps
  secretHash <- new$ parameter #! #lpSecretHash
  whenSome (swaps #: secretHash) $ \_ -> failCustom #swapLockAlreadyExists secretHash

  swapTransfer @s sender (FromAddress ()) $ parameter #! #lpAmount + parameter #! #lpFee

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
     , HasManagedLedgerStorage s
     )
  => IndigoEntrypoint sp
confirmSwap parameter = do
  ML.ensureNotPaused @s

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
  ML.ensureNotPaused @s

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

       swapTransfer @s (s #! #sTo) (ToAddress ()) $ s #! #sAmount + s #! #sFee

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
  ML.ensureNotPaused @s

  secretHash <- new$ parameter #! #crpSecretHash
  swaps <- getStorageField @s #swaps
  ifSome (swaps #: secretHash)
    (\s -> do
       when (sender /= s #! #sFrom) $ failCustom_ #senderIsNotTheInitiator
       when (now < s #! #sReleaseTime) $ failCustom #fundsLock $ s #! #sReleaseTime

       when (s #! #sFee /= 0 nat) $ swapTransfer @s (s #! #sTo) (ToAddress ()) $ s #! #sFee
       swapTransfer @s (s #! #sFrom) (ToAddress ()) $ s #! #sAmount

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

swapTransfer
  :: forall s addr value op.
     ( addr :~> Address, value :~> Natural, op :~> SwapTransferOperation
     , HasManagedLedgerStorage s
     , HasBridge s
     )
  => addr -> op -> value -> IndigoProcedure
swapTransfer address op value = do
  lockSaver <- getStorageField @s #lockSaver
  case_ op
    ( #cFromAddress #= \_ -> ML.debitFrom @s address value >> ML.creditTo @s lockSaver value
    , #cToAddress #= \_ -> ML.debitFrom @s lockSaver value >> ML.creditTo @s address value
    )
