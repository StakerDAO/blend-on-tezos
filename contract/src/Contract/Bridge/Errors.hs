module Contract.Bridge.Errors () where

import Indigo

import Fmt (Buildable (..), (+|), (|+))

import Contract.Bridge.Types (SwapId (..), TooLongSecretError (..))

type instance ErrorArg "swapLockAlreadyExists" = SwapId

type instance ErrorArg "swapLockDoesNotExist" = SwapId

type instance ErrorArg "secretHashIsAlreadySet" = SwapId

type instance ErrorArg "senderIsNotTheInitiator" = ()

type instance ErrorArg "tooLongSecret" = TooLongSecretError

type instance ErrorArg "swapIsOver" = Timestamp

type instance ErrorArg "fundsLock" = Timestamp

type instance ErrorArg "wrongOutcomeStatus" = MText

type instance ErrorArg "invalidSecret" = ()

instance Buildable (CustomError "swapLockAlreadyExists") where
  build (CustomError _ swapId) =
    "Swap lock with " +| swapId |+ " id already exists"

instance Buildable (CustomError "swapLockDoesNotExist") where
  build (CustomError _ swapId) =
    "Swap lock with " +| swapId |+ " id does not exists"

instance Buildable (CustomError "secretHashIsAlreadySet") where
  build (CustomError _ swapId) =
    "Secret hash for swap with " +| swapId |+ " id is already set"

instance Buildable (CustomError "senderIsNotTheInitiator") where
  build (CustomError _ _) =
    "You are not the initiator of this swap"

instance Buildable (CustomError "tooLongSecret") where
  build (CustomError _ TooLongSecretError {..}) =
    "Expected secret length limit is " +| tlseExpected |+
    " but actual is " +| tlseActual |+ ""

instance Buildable (CustomError "swapIsOver") where
  build (CustomError _ ts) =
    "Swap was ended at " +| ts |+ "."

instance Buildable (CustomError "fundsLock") where
  build (CustomError _ ts) =
    "Funds are still lock and swap will end at " +| ts |+ "."

instance Buildable (CustomError "wrongOutcomeStatus") where
  build (CustomError _ st) =
    "" +| st |+ " outcome status is not valid for this entrypoint."

instance Buildable (CustomError "invalidSecret") where
  build (CustomError _ _) =
    "Hash of the secret doesn't match outcome's secret hash."

instance CustomErrorHasDoc "swapLockAlreadyExists" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Lock with this id already exists"
  customErrArgumentSemantics = Just "swap id"

instance CustomErrorHasDoc "swapLockDoesNotExist" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Lock with this id does not exists"
  customErrArgumentSemantics = Just "swap id"

instance CustomErrorHasDoc "secretHashIsAlreadySet" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Secret hash is already set for swap with certain id"
  customErrArgumentSemantics = Just "swap id"

instance CustomErrorHasDoc "senderIsNotTheInitiator" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Sender is not the initiator of this swap"

instance CustomErrorHasDoc "tooLongSecret" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Secret length in longer then its limit"
  customErrArgumentSemantics = Just "actual and expected length"

instance CustomErrorHasDoc "swapIsOver" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Swap time is over"
  customErrArgumentSemantics = Just "timestamp"

instance CustomErrorHasDoc "fundsLock" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Funds are still lock"
  customErrArgumentSemantics = Just "timestamp"

instance CustomErrorHasDoc "wrongOutcomeStatus" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Not valid outcome status"
  customErrArgumentSemantics = Just "outcome status"

instance CustomErrorHasDoc "invalidSecret" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Invalid secret hash"
