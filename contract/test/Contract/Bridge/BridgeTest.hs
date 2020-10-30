module Contract.Bridge.BridgeTest
  ( test_Bridge
  ) where

import Prelude

import Data.Map (lookup, (!))
import Hedgehog (forAll)
import Hedgehog.Gen.Tezos.Core (maxTimestamp, minTimestamp)
import Lorentz (mkView)
import Lorentz.Test (contractConsumer, expectError, lCallDef, lExpectCustomError,
                     lExpectCustomError_, lExpectStorage, lExpectViewConsumerStorage,
                     lOriginateEmpty, withSender)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Util.Named ((.!))

import Contract.BlndOnTezos (Parameter (..), Storage (..))
import Contract.Bridge (ClaimRefundParams (..), ConfirmSwapParams (..), LockParams (..),
                        Outcome (..), RedeemParams (..), Swap (..), TooLongSecretError (..))
import qualified Contract.Bridge.Impl as CB
import Contract.Gen (genLock, genLongSecret, genRedeem, genSecretHash)
import Contract.TestSetup (integrationalTestContract, withBridgeContractP)
import Contract.TestUtil (OrigParams (..), checkThat, getBalance, getOutcomes, getSwaps,
                          getTotalSupply, lookupE, shouldBe)

test_Bridge :: [TestTree]
test_Bridge =
  [ testGroup "Lock entrypoint"
    [ testProperty "Initiator lock" $
        withBridgeContractP 10 $ \contractM OrigParams{..} -> do
          lock@LockParams{..} <- forAll $ genLock True opBob
          integrationalTestContract contractM $ \c -> do
            withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
            lExpectStorage @Storage c $ \st -> do
              let actualAliceBalance = getBalance opAlice st
              let actualLockSaverBalance = getBalance opLockSaver st
              let actualTotalSupply = getTotalSupply st
              actualSwap <- lookupE lpSecretHash $ getSwaps st
              checkThat "Total supply didn't changed" $
                actualTotalSupply `shouldBe` 2000
              checkThat "Balance was locked for sender" $
                actualAliceBalance `shouldBe` (opBalances ! opAlice - 110)
              checkThat "Locker balance increased" $
                actualLockSaverBalance `shouldBe` (opBalances ! opLockSaver + 110)
              checkThat "Swap match lock params" $
                actualSwap `shouldBe` Swap
                  { sFrom        = opAlice
                  , sTo          = opBob
                  , sAmount      = lpAmount
                  , sReleaseTime = lpReleaseTime
                  , sFee         = lpFee
                  , sConfirmed   = lpConfirmed
                  }
              checkThat "Outcome doesn't exists" $
                lookup lpSecretHash (getOutcomes st) `shouldBe` Nothing

    , testProperty "Not initiator lock" $
        withBridgeContractP 10 $ \contractM OrigParams{..} -> do
          lock@LockParams{..} <- forAll $ genLock False opBob
          integrationalTestContract contractM $ \c -> do
            withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
            lExpectStorage @Storage c $ \st -> do
              let actualAliceBalance = getBalance opAlice st
              let actualLockSaverBalance = getBalance opLockSaver st
              let actualTotalSupply = getTotalSupply st
              actualSwap <- lookupE lpSecretHash $ getSwaps st
              checkThat "Total supply didn't schanged" $
                actualTotalSupply `shouldBe` 2000
              checkThat "Balance was locked for sender" $
                actualAliceBalance `shouldBe` (opBalances ! opAlice - 100)
              checkThat "Locker balance increased" $
                actualLockSaverBalance `shouldBe` (opBalances ! opLockSaver + 100)
              checkThat "Swap match lock params" $
                actualSwap `shouldBe` Swap
                  { sFrom        = opAlice
                  , sTo          = opBob
                  , sAmount      = lpAmount
                  , sReleaseTime = lpReleaseTime
                  , sFee         = lpFee
                  , sConfirmed   = lpConfirmed
                  }
              checkThat "Outcome doesn't exists" $
                lookup lpSecretHash (getOutcomes st) `shouldBe` Nothing

    , testProperty "Lock with amount greater then locker balance failed" $
        withBridgeContractP 10 $ \contractM OrigParams{..} -> do
          gLock <- forAll $ genLock False opBob
          let lock@LockParams{..} = gLock{lpAmount = 3000}
          integrationalTestContract contractM $ \c -> do
            err <- expectError $ withSender opAlice $ lCallDef c $ Bridge $ CB.Lock lock
            lExpectCustomError #notEnoughBalance (#required .! 3000, #present .! 1000) err

    , testProperty "Lock with the same swap id fails" $
        withBridgeContractP 10 $ \contractM OrigParams{..} -> do
          lock@LockParams{..} <- forAll $ genLock False opBob
          integrationalTestContract contractM $ \c -> do
            withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
            err <- expectError $ withSender opAlice $ lCallDef c $ Bridge $ CB.Lock lock
            lExpectCustomError #swapLockAlreadyExists lpSecretHash err
    ]

  , testGroup "Confirm swap entrypoint"
      [ testProperty "Confirm swap confirm the swap" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            lock@LockParams{..} <- forAll $ genLock True opBob
            let confirmSwapParams = ConfirmSwapParams lpSecretHash
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              lExpectStorage @Storage c $ \st -> do
                checkThat "Outcome doesn't exists" $
                  lookup lpSecretHash (getOutcomes st) `shouldBe` Nothing
              withSender opAlice . lCallDef c $ Bridge $ CB.ConfirmSwap confirmSwapParams
              lExpectStorage @Storage c $ \st -> do
                actualSwap <- lookupE lpSecretHash $ getSwaps st
                checkThat "Swap confirmed" $
                  actualSwap `shouldBe` Swap
                    { sFrom        = opAlice
                    , sTo          = opBob
                    , sAmount      = lpAmount
                    , sReleaseTime = lpReleaseTime
                    , sFee         = lpFee
                    , sConfirmed   = True
                    }

      , testProperty "Confirm swap fails if swap doesn't exists" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            secretHash <- forAll genSecretHash
            let confirmSwapParams = ConfirmSwapParams secretHash
            integrationalTestContract contractM $ \c -> do
              err <- expectError $ withSender opAlice . lCallDef c $
                Bridge $ CB.ConfirmSwap confirmSwapParams
              lExpectCustomError #swapLockDoesNotExist secretHash err

      , testProperty "Confirm swap if sender is not the initiator" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            lock@LockParams{..} <- forAll $ genLock True opBob
            let confirmSwapParams = ConfirmSwapParams lpSecretHash
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              lExpectStorage @Storage c $ \st -> do
                checkThat "Outcome doesn't exists" $
                  lookup lpSecretHash (getOutcomes st) `shouldBe` Nothing
              err <- expectError $ withSender opBob . lCallDef c $
                Bridge $ CB.ConfirmSwap confirmSwapParams
              lExpectCustomError_ #senderIsNotTheInitiator err

      , testProperty "Confirm swap fails if swap was already confirmed" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            lock@LockParams{..} <- forAll $ genLock True opBob
            let confirmSwapParams = ConfirmSwapParams lpSecretHash
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              withSender opAlice . lCallDef c $ Bridge $ CB.ConfirmSwap confirmSwapParams
              err <- expectError $ withSender opAlice . lCallDef c $
                Bridge $ CB.ConfirmSwap confirmSwapParams
              lExpectCustomError #swapIsAlreadyConfirmed lpSecretHash err
      ]

  , testGroup "Redeem entrypoint"
      [ testProperty "Redeem works for non initiator" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock True opBob
            (redeem@RedeemParams{..}, sh) <- forAll genRedeem
            let lock@LockParams{..} = gLock {lpSecretHash = sh}
            let confirmSwapParams = ConfirmSwapParams lpSecretHash
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              withSender opAlice . lCallDef c $ Bridge $ CB.ConfirmSwap confirmSwapParams
              withSender opBob . lCallDef c $ Bridge $ CB.Redeem redeem
              lExpectStorage @Storage c $ \st -> do
                actualOutcome <- lookupE lpSecretHash $ getOutcomes st
                let actualBobBalance = getBalance opBob st
                let actualLockSaverBalance = getBalance opLockSaver st
                let actualTotalSupply = getTotalSupply st
                checkThat "Outcome secret revealed" $
                  actualOutcome `shouldBe` Outcome rpSecret
                checkThat "Total supply didn't schanged" $
                  actualTotalSupply `shouldBe` 2000
                checkThat "Bob balance changed" $
                  actualBobBalance `shouldBe` (opBalances ! opBob + 110)
                checkThat "Locker balance changed" $
                  actualLockSaverBalance `shouldBe` (opBalances ! opLockSaver)

      , testProperty "Redeem works for non initiator" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock False opBob
            (redeem@RedeemParams{..}, sh) <- forAll genRedeem
            let lock@LockParams{..} = gLock {lpSecretHash = sh}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              withSender opBob . lCallDef c $ Bridge $ CB.Redeem redeem
              lExpectStorage @Storage c $ \st -> do
                actualOutcome <- lookupE lpSecretHash $ getOutcomes st
                let actualBobBalance = getBalance opBob st
                let actualLockSaverBalance = getBalance opLockSaver st
                let actualTotalSupply = getTotalSupply st
                checkThat "Outcome secret revealed" $
                  actualOutcome `shouldBe` Outcome rpSecret
                checkThat "Total supply didn't schanged" $
                  actualTotalSupply `shouldBe` 2000
                checkThat "Bob balance changed" $
                  actualBobBalance `shouldBe` (opBalances ! opBob + 100)
                checkThat "Locker balance changed" $
                  actualLockSaverBalance `shouldBe` (opBalances ! opLockSaver)

      , testProperty "Redeem fails with long secret" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock True opBob
            (gRedeem, _) <- forAll genRedeem
            (s, sh) <- forAll genLongSecret
            let lock@LockParams{..} = gLock {lpSecretHash = sh}
                redeem@RedeemParams{..} = gRedeem {rpSecret = s}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem redeem
              lExpectCustomError #tooLongSecret (TooLongSecretError 32 64) err

      , testProperty "Redeem fails if swap doesn't exists" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            LockParams{..} <- forAll $ genLock False opBob
            (redeem@RedeemParams{..}, sh) <- forAll genRedeem
            integrationalTestContract contractM $ \c -> do
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem redeem
              lExpectCustomError #swapLockDoesNotExist sh err

      , testProperty "Redeem fails if swap was finished" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock False opBob
            (redeem@RedeemParams{..}, sh) <- forAll genRedeem
            let lock@LockParams{..} = gLock {lpSecretHash = sh}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              withSender opBob . lCallDef c $ Bridge $ CB.Redeem redeem
              err <- expectError $ withSender opBob . lCallDef c $ Bridge $ CB.Redeem redeem
              lExpectCustomError #swapIsOver lpSecretHash err
      ]

  , testGroup "Refund entrypoint"
      [ testProperty "Refund works" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock True opBob
            let refund = ClaimRefundParams lpSecretHash
                lock@LockParams{..} = gLock {lpReleaseTime = minTimestamp}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              withSender opAlice . lCallDef c $ Bridge $ CB.ClaimRefund refund
              lExpectStorage @Storage c $ \st -> do
                let actualAliceBalance = getBalance opAlice st
                let actualBobBalance = getBalance opBob st
                let actualLockSaverBalance = getBalance opLockSaver st
                let actualTotalSupply = getTotalSupply st
                checkThat "Outcome doesn't exists" $
                  lookup lpSecretHash (getOutcomes st) `shouldBe` Nothing
                checkThat "Total supply didn't schanged" $
                  actualTotalSupply `shouldBe` 2000
                checkThat "Alice balance changed" $
                  actualAliceBalance `shouldBe` (opBalances ! opAlice - 10)
                checkThat "Bob balance changed" $
                  actualBobBalance `shouldBe` (opBalances ! opBob + 10)
                checkThat "Locker balance didn't changed" $
                  actualLockSaverBalance `shouldBe` (opBalances ! opLockSaver)

      , testProperty "Refund fails if swap doesn't exists" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            secretHash <- forAll genSecretHash
            let refund = ClaimRefundParams secretHash
            integrationalTestContract contractM $ \c -> do
              err <- expectError $ withSender opAlice . lCallDef c $
                Bridge $ CB.ClaimRefund refund
              lExpectCustomError #swapLockDoesNotExist secretHash err

      , testProperty "Refund fails if swap is not over" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock True opBob
            let refund = ClaimRefundParams lpSecretHash
                lock@LockParams{..} = gLock {lpReleaseTime = maxTimestamp}
            integrationalTestContract contractM $ \c -> do
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              err <- expectError $ withSender opAlice . lCallDef c $
                Bridge $ CB.ClaimRefund refund
              lExpectCustomError #fundsLock maxTimestamp err
      ]

  , testGroup "View entrypoints"
      [ testProperty "Get swap and outcome" $
          withBridgeContractP 10 $ \contractM OrigParams{..} -> do
            gLock <- forAll $ genLock False opBob
            (redeem@RedeemParams{..}, sh) <- forAll genRedeem
            let lock@LockParams{..} = gLock {lpSecretHash = sh}
            integrationalTestContract contractM $ \c -> do
              swapConsumer <- lOriginateEmpty @(Maybe Swap) contractConsumer "consumer"
              outcomeConsumer <- lOriginateEmpty @(Maybe Outcome) contractConsumer "consumer"
              withSender opAlice . lCallDef c $ Bridge $ CB.Lock lock
              lCallDef c $ Bridge $ CB.GetSwap $ mkView lpSecretHash swapConsumer
              lExpectViewConsumerStorage swapConsumer
                [ Just Swap
                  { sFrom        = opAlice
                  , sTo          = opBob
                  , sAmount      = lpAmount
                  , sReleaseTime = lpReleaseTime
                  , sFee         = lpFee
                  , sConfirmed   = lpConfirmed
                  }
                ]
              withSender opBob . lCallDef c $ Bridge $ CB.Redeem redeem
              lCallDef c $ Bridge $ CB.GetOutcome $ mkView lpSecretHash outcomeConsumer
              lExpectViewConsumerStorage outcomeConsumer
                [ Just $ Outcome rpSecret
                ]
      ]
  ]
