module Contract.Bridge.Errors () where

import Indigo

import Data.Coerce (coerce)
import Fmt (Buildable (..), (+|), (|+))

import Contract.Bridge.Types (SwapId (..))

type instance ErrorArg "swapLockAlreadyExists" = SwapId

type instance ErrorArg "swapLockDoesNotExists" = SwapId

type instance ErrorArg "secreteHashIsAlreadySet" = SwapId

type instance ErrorArg "senderIsNotTheInitiator" = ()

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
