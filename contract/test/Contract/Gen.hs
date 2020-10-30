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
import Hedgehog.Gen (bytes, discard, integral)
import Hedgehog.Gen.Tezos.Core (maxTimestamp)
import Hedgehog.Gen.Tezos.Crypto (genKeyHash)
import Hedgehog.Range (linear, singleton)
import Lorentz (Address)
import Tezos.Address (Address (..))
import Tezos.Crypto (blake2b)

import Contract.Bridge (LockParams (..), RedeemParams (..), SecretHash (..))
import Contract.TestUtil (OrigParams (..))

tokenBase :: Natural
tokenBase = 10 ^ (18 :: Natural)

-- TODO set from to 0, after ManagedLedger contract fix
genNatural :: MonadGen m => Natural -> Natural -> m Natural
genNatural from to = integral @_ @Natural (linear from to)

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
  (aliceAddress, aliceBalance) <- mzip genAddress $ genNatural 1 1000
  (bobAddress, bobBalance) <- mzip genAddress $ genNatural 1 1000
  lockSaverAddress <- genAddress
  when ( aliceAddress == bobAddress
      || bobAddress == lockSaverAddress
      || aliceAddress == lockSaverAddress
       ) discard
  pure OrigParams
    { opBalances = fromList
      [ (aliceAddress, aliceBalance * tokenBase)
      , (bobAddress, bobBalance * tokenBase)
      , (lockSaverAddress, 0)
      ]
    , opAlice = aliceAddress
    , opBob = bobAddress
    , opLockSaver = lockSaverAddress
    }
  where
    mzip ma mb = do
      a <- ma
      b <- mb
      pure (a, b)

genLock :: MonadGen m => Bool -> Natural -> Address -> m LockParams
genLock isInitiator lb to = do
  let lockerBalance = lb `div` tokenBase
  let ts = maxTimestamp
  amount <- genNatural 1 lockerBalance
  fee <- (\n -> bool 0 n isInitiator) <$> genNatural 0 (lockerBalance - amount)
  secretHash <- genSecretHash
  pure LockParams
    { lpTo          = to
    , lpAmount      = tokenBase * amount
    , lpReleaseTime = ts
    , lpSecretHash  = secretHash
    , lpFee         = tokenBase * fee
    , lpConfirmed   = not isInitiator
    }

genRedeem :: MonadGen m => m (RedeemParams, SecretHash)
genRedeem = do
  rpSecret <- genByteString
  let secreteHash = blake2b rpSecret
  pure (RedeemParams {..}, DC.coerce secreteHash)

