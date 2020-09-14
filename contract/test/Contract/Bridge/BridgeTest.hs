module Contract.Bridge.BridgeTest
  ( test_Bridge
  ) where

import Prelude

import Data.Map (lookup, (!))
import Hedgehog (forAll)
import Hedgehog.Gen.Tezos.Core (minTimestamp)
import Lorentz (arg, mt)
import Lorentz.Test (expectError, lCallDef, lExpectCustomError, lExpectCustomError_, lExpectStorage,
                     withSender)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Util.Named ((.!))

import Contract.BlndOnTezos (Parameter (..), Storage (..))
import Contract.Bridge (ClaimRefundParams (..), LockParams (..), Outcome (..), RedeemParams (..),
                        RevealSecretHashParams (..), Swap (..), TooLongSecretError (..))
import qualified Contract.Bridge.Impl as CB
import Contract.Gen (genLock, genLongSecret, genRedeem, genRevealSecretHash, genSwapId)
import Contract.TestSetup (integrationalTestContract, withBridgeContractP)
import Contract.TestUtil (OrigParams (..), checkThat, getLedger, getOutcomes, getSwaps,
                          getTotalSupply, lookupE, shouldBe)

test_Bridge :: [TestTree]
test_Bridge =
  [ testGroup "Lock entrypoint"
    [ testProperty "Lock with secret hash" $
        withBridgeContractP 10 $ \contractM OrigParams{..} -> do
          lock@LockParams{..} <- forAll $ genLock True opBob
          integrationalTestContract contractM $ \c -> do
            withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
            lExpectStorage @Storage c $ \st -> do
              (arg #balance -> actualBalance) <- lookupE opAlice $ getLedger st
              actualSwap <- lookupE lpId $ getSwaps st
              actualOutcome <- lookupE lpId $ getOutcomes st
              actualTotalSupply <- pure $ getTotalSupply st
              checkThat "Total supply didn't schanged" $
                actualTotalSupply `shouldBe` 2000
              checkThat "Balance was locked for sender" $
                actualBalance `shouldBe` (opBalances ! opAlice - 100)
              checkThat "Swap match lock params" $
                actualSwap `shouldBe` Swap
                  { sFrom        = opAlice
                  , sTo          = opBob
                  , sAmount      = lpAmount
                  , sReleaseTime = lpReleaseTime
                  }
              checkThat "Outcome match lock params" $
                actualOutcome `shouldBe` HashRevealed (fromMaybe "" lpSecretHash)

    , testProperty "Lock without secret hash" $
        withBridgeContractP 10 $ \contractM OrigParams{..} -> do
          lock@LockParams{..} <- forAll $ genLock False opBob
          integrationalTestContract contractM $ \c -> do
            withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
            lExpectStorage @Storage c $ \st -> do
              (arg #balance -> actualBalance) <- lookupE opAlice $ getLedger st
              actualSwap <- lookupE lpId $ getSwaps st
              actualTotalSupply <- pure $ getTotalSupply st
              checkThat "Total supply didn't schanged" $
                actualTotalSupply `shouldBe` 2000
              checkThat "Balance was locked for sender" $
                actualBalance `shouldBe` (opBalances ! opAlice - 100)
              checkThat "Swap match lock params" $
                actualSwap `shouldBe` Swap
                  { sFrom        = opAlice
                  , sTo          = opBob
                  , sAmount      = lpAmount
                  , sReleaseTime = lpReleaseTime
                  }
              checkThat "Outcome doesn't exists" $
                lookup lpId (getOutcomes st) `shouldBe` Nothing

    , testProperty "Lock with amount greater then locker balance failed" $
        withBridgeContractP 10 $ \contractM OrigParams{..} -> do
          gLock <- forAll $ genLock False opBob
          let lock@LockParams{..} = gLock{lpAmount = 3000}
          integrationalTestContract contractM $ \c -> do
            err <- expectError $ withSender opAlice $ lCallDef c $ Bridge $ CB.Lock $ lock
            lExpectCustomError #notEnoughBalance (#required .! 3000, #present .! 1000) err

    , testProperty "Lock with the same swap id fails" $
        withBridgeContractP 10 $ \contractM OrigParams{..} -> do
          lock@LockParams{..} <- forAll $ genLock False opBob
          integrationalTestContract contractM $ \c -> do
            withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
            err <- expectError $ withSender opAlice $ lCallDef c $ Bridge $ CB.Lock $ lock
            lExpectCustomError #swapLockAlreadyExists lpId err
    ]

  , testGroup "Reveal secret hash entrypoint"
      [ testProperty "Reveal secret hash set the hash" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            lock@LockParams{..} <- forAll $ genLock False opBob
            rsh@RevealSecretHashParams{..} <- forAll $ genRevealSecretHash lpId
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              lExpectStorage @Storage c $ \st -> do
                checkThat "Outcome doesn't exists" $
                  lookup lpId (getOutcomes st) `shouldBe` Nothing
              withSender opAlice . lCallDef c $ Bridge $ CB.RevealSecretHash $ rsh
              lExpectStorage @Storage c $ \st -> do
                actualOutcome <- lookupE lpId $ getOutcomes st
                checkThat "Outcome exists and contains secret hash" $
                  actualOutcome `shouldBe` HashRevealed rshpSecretHash

      , testProperty "Reveal secret hash fails if swap doesn't exists" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            swapId <- forAll $ genSwapId
            rsh@RevealSecretHashParams{..} <- forAll $ genRevealSecretHash swapId
            integrationalTestContract contractM $ \c -> do
              err <- expectError $ withSender opAlice . lCallDef c $
                Bridge $ CB.RevealSecretHash $ rsh
              lExpectCustomError #swapLockDoesNotExists rshpId err

      , testProperty "Reveal secret hash fails if sender is not the initiator" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            lock@LockParams{..} <- forAll $ genLock False opBob
            rsh@RevealSecretHashParams{..} <- forAll $ genRevealSecretHash lpId
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              lExpectStorage @Storage c $ \st -> do
                checkThat "Outcome doesn't exists" $
                  lookup lpId (getOutcomes st) `shouldBe` Nothing
              err <- expectError $ withSender opBob . lCallDef c $
                Bridge $ CB.RevealSecretHash $ rsh
              lExpectCustomError_ #senderIsNotTheInitiator err

      , testProperty "Reveal secret hash fails if hash exists" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            lock@LockParams{..} <- forAll $ genLock True opBob
            rsh@RevealSecretHashParams{..} <- forAll $ genRevealSecretHash lpId
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              err <- expectError $ withSender opAlice . lCallDef c $
                Bridge $ CB.RevealSecretHash $ rsh
              lExpectCustomError #secretHashIsAlreadySet lpId err
      ]

  , testGroup "Redeem entrypoint"
      [ testProperty "Redeem works" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock True opBob
            (redeem@RedeemParams{..}, sh) <- forAll $ genRedeem $ lpId gLock
            let lock@LockParams{..} = gLock {lpSecretHash = Just sh}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              lExpectStorage @Storage c $ \st -> do
                actualOutcome <- lookupE lpId $ getOutcomes st
                (arg #balance -> actualBalance) <- lookupE opBob $ getLedger st
                actualTotalSupply <- pure $ getTotalSupply st
                checkThat "Outcome secret revealed" $
                  actualOutcome `shouldBe` SecretRevealed rpSecret
                checkThat "Total supply didn't schanged" $
                  actualTotalSupply `shouldBe` 2000
                checkThat "Balance was locked for sender" $
                  actualBalance `shouldBe` (opBalances ! opBob + 100)

      , testProperty "Redeem fails with long secret" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock True opBob
            (gRedeem, _) <- forAll $ genRedeem $ lpId gLock
            (s, sh) <- forAll genLongSecret
            let lock@LockParams{..} = gLock {lpSecretHash = Just sh}
                redeem@RedeemParams{..} = gRedeem {rpSecret = s}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              lExpectCustomError #tooLongSecret (TooLongSecretError 32 64) err

      , testProperty "Redeem fails if swap doesn't exists" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            LockParams{..} <- forAll $ genLock False opBob
            (redeem@RedeemParams{..}, _) <- forAll $ genRedeem lpId
            integrationalTestContract contractM $ \c -> do
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              lExpectCustomError #swapLockDoesNotExists lpId err

      , testProperty "Redeem fails if swap is over" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock True opBob
            (redeem@RedeemParams{..}, sh) <- forAll $ genRedeem $ lpId gLock
            let lock@LockParams{..} = gLock {lpSecretHash = Just sh, lpReleaseTime = minTimestamp}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              lExpectCustomError #swapIsOver minTimestamp err

      , testProperty "Redeem fails if outcome doesn't exists" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            lock@LockParams{..} <- forAll $ genLock False opBob
            (redeem@RedeemParams{..}, _) <- forAll $ genRedeem lpId
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              lExpectCustomError #swapLockDoesNotExists lpId err

      , testProperty "Redeem fails with invalid secret" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            lock@LockParams{..} <- forAll $ genLock True opBob
            (redeem@RedeemParams{..}, _) <- forAll $ genRedeem lpId
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              lExpectCustomError_ #invalidSecret err

      , testProperty "Redeem fails if swap was finished" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock False opBob
            (redeem@RedeemParams{..}, sh) <- forAll $ genRedeem $ lpId gLock
            let lock@LockParams{..} = gLock {lpSecretHash = Just sh}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              lExpectCustomError #wrongOutcomeStatus [mt|SecretRevealed|] err

      , testProperty "Redeem fails if swap was refunded" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock False opBob
            (redeem@RedeemParams{..}, sh) <- forAll $ genRedeem $ lpId gLock
            let lock@LockParams{..} = gLock {lpSecretHash = Just sh, lpReleaseTime = minTimestamp}
                refund = ClaimRefundParams lpId
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock $ lock
              withSender opAlice . lCallDef c $ Bridge $ CB.ClaimRefund $ refund
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem $ redeem
              lExpectCustomError #wrongOutcomeStatus [mt|Refunded|] err
      ]
  ]

--"Refund set outcome to Refund and return money to Alice"
--"Fail if Swap not exists or outcome exists and not HashRevealed"
--"Fail if Swap if over"
--
-- Get Swap
-- Get Outcome
