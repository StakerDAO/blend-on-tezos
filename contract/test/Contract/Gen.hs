module Contract.Gen
  ( genByteString
  , genAddress
  , genSwapId
  , genOrigParams
  , genLock
  , genRevealSecretHash
  ) where

import Prelude

import Data.Coerce (coerce)
import Data.Map (fromList)
import Hedgehog (MonadGen)
import Hedgehog.Gen (bytes, discard)
import Hedgehog.Gen.Tezos.Crypto (genKeyHash)
import Hedgehog.Range (singleton)
import Lorentz (Address)
import Tezos.Address (Address (..))

import Contract.Bridge (LockParams (..), RevealSecretHashParams (..), SwapId (..))
import Contract.TestUtil (OrigParams (..))
import Hedgehog.Gen.Tezos.Core (maxTimestamp)

genByteString :: MonadGen m => m ByteString
genByteString = bytes (singleton 32)

genAddress :: MonadGen m => m Address
genAddress = KeyAddress <$> genKeyHash

genSwapId :: MonadGen m => m SwapId
genSwapId = coerce <$> genByteString

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
genLock withSecret to = do
  swapId <- genSwapId
  secretHash <- genByteString
  let ts = maxTimestamp
  pure LockParams
    { lpId          = coerce swapId
    , lpTo          = to
    , lpAmount      = 100
    , lpReleaseTime = ts
    , lpSecretHash  = bool Nothing (Just secretHash) withSecret
    }

genRevealSecretHash :: MonadGen m => SwapId -> m RevealSecretHashParams
genRevealSecretHash rshpId = do
  rshpSecretHash <- genByteString
  pure $ RevealSecretHashParams {..}
