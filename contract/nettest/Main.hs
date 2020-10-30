module Main
  ( main
  ) where

import Prelude

import qualified Data.Coerce as DC

import Data.Map (fromList)
import Hedgehog.Gen.Tezos.Core (maxTimestamp)
import Lorentz (Address, Contract, EntrypointRef (..), NiceStorage)
import Morley.Nettest (AddressOrAlias (..), NettestImpl, NettestScenario, callFrom, comment,
                       mkNettestEnv, nettestConfigParser, newAddress, niComment, originateSimple,
                       parserInfo, resolveNettestAddress, runNettestClient,
                       runNettestViaIntegrational, uncapsNettest)
import qualified Morley.Nettest.ApprovableLedger as ML
import Options.Applicative (execParser)
import Tezos.Core (Timestamp (..))
import Util.Exception (displayUncaughtException)
import Util.Named ((.!))

import Contract.BlndOnTezos (Parameter, blndOnTezosContract, mkStorage)
import Contract.Bridge (ClaimRefundParams (..), ConfirmSwapParams (..), LockParams (..),
                        RedeemParams (..), SecretHash (..))
import Tezos.Crypto (blake2b)

main :: IO ()
main = displayUncaughtException do
  cfg <- execParser $ parserInfo
    (#usage .! mempty)
    (#description .! "BlndOnTezos nettest scenario")
    (#header .! "BlndOnTezos nettest")
    (#parser .! (nettestConfigParser . pure $ Just "nettest.BlndOnTezos"))
  env <- mkNettestEnv cfg
  runNettestViaIntegrational scenario
  runNettestClient env scenario
  where
    scenario :: NettestScenario m
    scenario impl = do
      niComment impl "Token scenario"
      ML.simpleScenario (\addr -> mkStorage addr addr mempty) blndOnTezosContract impl
      niComment impl "Bridge scenario"
      simpleScenario (\alice bob lockSaver ->
        mkStorage alice lockSaver $ fromList [(alice, 1000), (bob, 1000)]) blndOnTezosContract impl

simpleScenario
  :: forall m storage.
     ( Monad m
     , NiceStorage storage
     )
  => (Address -> Address -> Address -> storage)
  -> Contract Parameter storage
  -> NettestImpl m
  -> m ()
simpleScenario mkInitialStorage contract = uncapsNettest $ do
  aliceAddr <- resolveNettestAddress
  bobAddr <- newAddress "bob"
  lockSaverAddress <- newAddress "lockSaver"
  c <- originateSimple "BlndOnTezos" (mkInitialStorage aliceAddr bobAddr lockSaverAddress) contract
  let secret = ("secret" :: ByteString)
      secretHash1 = SecretHash $ blake2b $ DC.coerce secret
      secretHash2 = SecretHash ("secretHash2" :: ByteString)
      alice = AddressResolved aliceAddr
      bob = AddressResolved bobAddr

  comment "Lock"
  callFrom alice c (Call @"Lock") $ LockParams
    { lpSecretHash  = secretHash1
    , lpTo          = bobAddr
    , lpAmount      = 100
    , lpReleaseTime = maxTimestamp
    , lpFee         = 10
    , lpConfirmed   = False
    }

  comment "ConfirmSwap"
  callFrom alice c (Call @"ConfirmSwap") $ ConfirmSwapParams secretHash1

  comment "Redeem"
  callFrom bob c (Call @"Redeem") $ RedeemParams secret

  comment "Lock in the past"
  callFrom alice c (Call @"Lock") $ LockParams
    { lpSecretHash  = secretHash2
    , lpTo          = bobAddr
    , lpAmount      = 100
    , lpReleaseTime = Timestamp 10
    , lpFee         = 10
    , lpConfirmed   = False
    }

  comment "Refund"
  callFrom alice c (Call @"ClaimRefund") $ ClaimRefundParams secretHash2
