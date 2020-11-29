module Contract.Bridge.Errors () where

import Indigo

import Fmt (Buildable (..), (+|), (|+))

import Contract.Bridge.Types (SecretHash (..), TooLongSecretError (..))

type instance ErrorArg "swapLockAlreadyExists" = SecretHash

type instance ErrorArg "swapLockDoesNotExist" = SecretHash

type instance ErrorArg "swapIsAlreadyConfirmed" = SecretHash

type instance ErrorArg "senderIsNotTheInitiator" = ()

type instance ErrorArg "tooLongSecret" = TooLongSecretError

type instance ErrorArg "swapIsNotConfirmed" = SecretHash

type instance ErrorArg "fundsLock" = Timestamp

type instance ErrorArg "swapIsOver" = SecretHash

instance Buildable (CustomError "swapLockAlreadyExists") where
  build (CustomError _ secretHash) =
    "Swap lock with " +| secretHash |+ " hash already exists"

instance Buildable (CustomError "swapLockDoesNotExist") where
  build (CustomError _ secretHash) =
    "Swap lock with " +| secretHash |+ " hash does not exists"

instance Buildable (CustomError "swapIsAlreadyConfirmed") where
  build (CustomError _ secretHash) =
    "Swap with " +| secretHash |+ " hash is already confirmed"

instance Buildable (CustomError "senderIsNotTheInitiator") where
  build (CustomError _ _) =
    "You are not the initiator of this swap"

instance Buildable (CustomError "tooLongSecret") where
  build (CustomError _ TooLongSecretError {..}) =
    "Expected secret length limit is " +| tlseExpected |+
    " but actual is " +| tlseActual |+ ""

instance Buildable (CustomError "swapIsNotConfirmed") where
  build (CustomError _ secretHash) =
    "Swap with " +| secretHash |+ " hash is not confirmed."

instance Buildable (CustomError "fundsLock") where
  build (CustomError _ ts) =
    "Funds are still lock and swap will end at " +| ts |+ "."

instance Buildable (CustomError "swapIsOver") where
  build (CustomError _ secretHash) =
    "Swap with " +| secretHash |+ " hash is over."

instance CustomErrorHasDoc "swapLockAlreadyExists" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Lock with this hash already exists"
  customErrArgumentSemantics = Just "swap hash"

instance CustomErrorHasDoc "swapLockDoesNotExist" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Lock with this hash does not exists"
  customErrArgumentSemantics = Just "swap hash"

instance CustomErrorHasDoc "swapIsAlreadyConfirmed" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Swap is already confirmed"
  customErrArgumentSemantics = Just "swap hash"

instance CustomErrorHasDoc "senderIsNotTheInitiator" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Sender is not the initiator of this swap"

instance CustomErrorHasDoc "tooLongSecret" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Secret length in longer then its limit"
  customErrArgumentSemantics = Just "actual and expected length"

instance CustomErrorHasDoc "swapIsNotConfirmed" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Swap is not confirmed"
  customErrArgumentSemantics = Just "swap hash"

instance CustomErrorHasDoc "fundsLock" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Funds are still lock"
  customErrArgumentSemantics = Just "timestamp"

instance CustomErrorHasDoc "swapIsOver" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Swap is over"
  customErrArgumentSemantics = Just "swap hash"
