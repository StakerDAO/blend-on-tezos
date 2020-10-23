module Contract.Gen
  ( genByteString
  , genAddress
  , genSecretHash
  , genOrigParams
  , genLock
  , genRedeem
  , genLongSecret
  ) where

import Prelude

import qualified Data.Coerce as DC
import Data.Map (fromList)
import Hedgehog (MonadGen)
import Hedgehog.Gen (bytes, discard)
import Hedgehog.Gen.Tezos.Core (maxTimestamp)
import Hedgehog.Gen.Tezos.Crypto (genKeyHash)
import Hedgehog.Range (singleton)
import Lorentz (Address)
import Tezos.Address (Address (..))
import Tezos.Crypto (blake2b)

import Contract.Bridge (LockParams (..), RedeemParams (..), SecretHash (..))
import Contract.TestUtil (OrigParams (..))

genLongSecret :: MonadGen m => m (ByteString, SecretHash)
genLongSecret = do
  s <- bytes $ singleton 64
  let sh = blake2b s
  pure (s, DC.coerce sh)

genByteString :: MonadGen m => m ByteString
genByteString = bytes $ singleton 32

genAddress :: MonadGen m => m Address
genAddress = KeyAddress <$> genKeyHash

genSecretHash :: MonadGen m => m SecretHash
genSecretHash = DC.coerce <$> genByteString

genOrigParams :: MonadGen m => m OrigParams
genOrigParams = do
  aliceAddress <- genAddress
  bobAddress <- genAddress
  when (aliceAddress == bobAddress) discard
  pure OrigParams
    { opBalances = fromList [(aliceAddress, 1000), (bobAddress, 1000)]
    , opAlice = aliceAddress
    , opBob = bobAddress
    }

genLock :: MonadGen m => Bool -> Address -> m LockParams
genLock isInitiator to = do
  secretHash <- genSecretHash
  let ts = maxTimestamp
  pure LockParams
    { lpTo          = to
    , lpAmount      = 100
    , lpReleaseTime = ts
    , lpSecretHash  = secretHash
    , lpFee         = bool Nothing (Just 10) isInitiator
    , lpConfirmed   = not isInitiator
    }

genRedeem :: MonadGen m => m (RedeemParams, SecretHash)
genRedeem = do
  rpSecret <- genByteString
  let secreteHash = blake2b rpSecret
  pure (RedeemParams {..}, DC.coerce secreteHash)

