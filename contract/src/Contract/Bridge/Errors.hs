module Contract.Bridge.Errors () where

import Indigo

import Data.Coerce (coerce)
import Fmt (Buildable (..), (+|), (|+))

import Contract.Bridge.Types (SwapId (..), TooLongSecretError (..))

type instance ErrorArg "swapLockAlreadyExists" = SwapId

type instance ErrorArg "swapLockDoesNotExists" = SwapId

type instance ErrorArg "secreteHashIsAlreadySet" = SwapId

type instance ErrorArg "senderIsNotTheInitiator" = ()

type instance ErrorArg "tooLongSecrete" = TooLongSecretError

type instance ErrorArg "swapIsOver" = Timestamp

type instance ErrorArg "wrongOutcomeStatus" = MText

instance Buildable (CustomError "swapLockAlreadyExists") where
  build (CustomError _ swapId) =
    "Swap lock with "
    +| decodeUtf8 @Text @ByteString (coerce swapId) |+
    " id already exists"

instance Buildable (CustomError "swapLockDoesNotExists") where
  build (CustomError _ swapId) =
    "Swap lock with "
    +| decodeUtf8 @Text @ByteString (coerce swapId) |+
    " id does not exists"

instance Buildable (CustomError "secreteHashIsAlreadySet") where
  build (CustomError _ swapId) =
    "Secrete hash for swap with "
    +| decodeUtf8 @Text @ByteString (coerce swapId) |+
    " id is already set"

instance Buildable (CustomError "senderIsNotTheInitiator") where
  build (CustomError _ _) =
    "You are not the initiator of this swap"

instance Buildable (CustomError "tooLongSecrete") where
  build (CustomError _ TooLongSecretError {..}) =
    "Expected secrete length limit is " +| tlseExpected |+
    " but actual is " +| tlseActual |+ ""
    
instance Buildable (CustomError "swapIsOver") where
  build (CustomError _ ts) =
    "Swap was ended at " +| ts |+ "."

instance Buildable (CustomError "wrongOutcomeStatus") where
  build (CustomError _ st) =
    "" +| st |+ " outcome status is not valid for this entrypoint."

instance CustomErrorHasDoc "swapLockAlreadyExists" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Lock with this id already exists"
  customErrArgumentSemantics = Just "swap id"

instance CustomErrorHasDoc "swapLockDoesNotExists" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Lock with this id does not exists"
  customErrArgumentSemantics = Just "swap id"

instance CustomErrorHasDoc "secreteHashIsAlreadySet" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Secrete hash is already set for swap with certain id"
  customErrArgumentSemantics = Just "swap id"

instance CustomErrorHasDoc "senderIsNotTheInitiator" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Sender is not the initiator of this swap"
    
instance CustomErrorHasDoc "tooLongSecrete" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Secret length in longer then its limit"
  customErrArgumentSemantics = Just "actual and expected length"

instance CustomErrorHasDoc "swapIsOver" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Swap time is over"
  customErrArgumentSemantics = Just "timestamp"
  
instance CustomErrorHasDoc "wrongOutcomeStatus" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Not valid outcome status"
  customErrArgumentSemantics = Just "outcome status"
