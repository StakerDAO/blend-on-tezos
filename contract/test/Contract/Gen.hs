module Contract.Gen
  ( genByteString
  , genAddress
  , genOrigParams
  , genLock
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

import Contract.Bridge (LockParams (..), SwapId (..))
import Contract.TestUtil (OrigParams (..))
import Hedgehog.Gen.Tezos.Core (genTimestamp)

genByteString :: MonadGen m => m ByteString
genByteString = bytes (singleton 32)

genAddress :: MonadGen m => m Address
genAddress = KeyAddress <$> genKeyHash

genOrigParams :: MonadGen m => m OrigParams
genOrigParams = do
  aliceAddress <- genAddress
  bobAddress <- genAddress
  when (aliceAddress == bobAddress) discard
  pure $ OrigParams
    { opBalances = fromList [(aliceAddress, 1000), (bobAddress, 1000)]
    , opAlice = aliceAddress
    , opBob = bobAddress
    }

genLock :: MonadGen m => Bool -> Address -> m LockParams
genLock withSecrete to = do
  swapId <- genByteString
  secreteHash <- genByteString
  ts <- genTimestamp
  pure LockParams
    { lpId          = coerce swapId
    , lpTo          = to
    , lpAmount      = 100
    , lpReleaseTime = ts
    , lpSecretHash  = bool Nothing (Just secreteHash) withSecrete
    }
