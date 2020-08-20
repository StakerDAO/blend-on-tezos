module Contract.Bridge.Errors () where

import Indigo

import Data.Coerce (coerce)
import Fmt (Buildable (..), (+|), (|+))

import Contract.Bridge.Types (SwapId (..))

type instance ErrorArg "swapLockAlreadyExists" = SwapId

instance Buildable (CustomError "swapLockAlreadyExists") where
  build (CustomError _ swapId) =
    "Swap lock with " +| decodeUtf8 @Text @ByteString (coerce swapId) |+ " id already exists"

instance CustomErrorHasDoc "swapLockAlreadyExists" where
  customErrClass = ErrClassActionException
  customErrDocMdCause =
    "Lock with this id already exists"
  customErrArgumentSemantics = Just "swap id"
