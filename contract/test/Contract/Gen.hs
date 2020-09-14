module Contract.Gen
  ( genByteString
  , genAddress
  , genSwapId
  , genOrigParams
  , genLock
  , genRevealSecretHash
  , genRedeem
  , genLongSecret
  ) where

import Prelude

import Data.Coerce (coerce)
import Data.Map (fromList)
import Hedgehog (MonadGen)
import Hedgehog.Gen (bytes, discard)
import Hedgehog.Gen.Tezos.Core (maxTimestamp)
import Hedgehog.Gen.Tezos.Crypto (genKeyHash)
import Hedgehog.Range (singleton)
import Lorentz (Address)
import Tezos.Address (Address (..))
import Tezos.Crypto (sha256)

import Contract.Bridge (LockParams (..), RedeemParams (..), RevealSecretHashParams (..),
                        SwapId (..))
import Contract.TestUtil (OrigParams (..))

genLongSecret :: MonadGen m => m (ByteString, ByteString)
genLongSecret = do
  s <- bytes $ singleton 64
  let sh = sha256 s
  pure (s, sh)

genByteString :: MonadGen m => m ByteString
genByteString = bytes $ singleton 32

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
  pure RevealSecretHashParams {..}

genRedeem :: MonadGen m => SwapId -> m (RedeemParams, ByteString)
genRedeem rpId = do
  rpSecret <- genByteString
  let secreteHash = sha256 rpSecret
  pure (RedeemParams {..}, secreteHash)

