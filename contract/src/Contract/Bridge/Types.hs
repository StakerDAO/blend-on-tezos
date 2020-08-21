module Contract.Bridge.Types
  ( LockParams (..)
  , SwapId (..)
  , Swap (..)
  , OutcomeStatus (..)
  , Outcome (..)
  , RevealSecretHashParams (..)
  ) where

import Indigo

newtype SwapId = SwapId ByteString
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc SwapId where
  typeDocMdDescription = "SwapId."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data Swap = Swap
  { sFrom        :: Address
  , sTo          :: Address
  , sAmount      :: Natural
  , sReleaseTime :: Timestamp
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Swap where
  typeDocMdDescription = "Swap storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data OutcomeStatus
  = Refunded
  | HashRevealed
  | SecretRevealed
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc OutcomeStatus where
  typeDocMdDescription = "OutcomeStatus."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data Outcome = Outcome
  { oStatus     :: OutcomeStatus
  , oSecret     :: Maybe ByteString
  , oSecretHash :: Maybe ByteString
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Outcome where
  typeDocMdDescription = "Outcome storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data LockParams = LockParams
  { lpId          :: SwapId
  , lpTo          :: Address
  , lpAmount      :: Natural
  , lpReleaseTime :: Timestamp
  , lpSecretHash  :: Maybe ByteString
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc LockParams where
  typeDocMdDescription = "Lock entrypoint params."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data RevealSecretHashParams = RevealSecretHashParams
  { rshpId          :: SwapId
  , rshpSecreteHash :: ByteString
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc RevealSecretHashParams where
  typeDocMdDescription = "RevealSecretHash entrypoint params."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

