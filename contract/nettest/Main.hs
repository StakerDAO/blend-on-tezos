module Main
  ( main
  ) where

import Prelude

import Data.Map (fromList)
import Hedgehog.Gen.Tezos.Core (maxTimestamp)
import Lorentz (Address, Contract, EntrypointRef (..), NiceStorage)
import Morley.Nettest (AddressOrAlias (..), NettestImpl, NettestScenario, NettestT, callFrom,
                       comment, mkNettestEnv, nettestConfigParser, newAddress, niComment,
                       originateSimple, parserInfo, resolveNettestAddress, runNettestClient,
                       runNettestViaIntegrational, uncapsNettest)
import qualified Morley.Nettest.ApprovableLedger as ML
import Options.Applicative (execParser)
import Tezos.Core (Timestamp (..))
import Tezos.Crypto (sha256)
import Util.Exception (displayUncaughtException)
import Util.Named ((.!))

import Contract.BlndOnTezos (Parameter, blndOnTezosContract, mkStorage)
import Contract.Bridge (ClaimRefundParams (..), LockParams (..), RedeemParams (..),
                        RevealSecretHashParams (..), SwapId (..))

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
      ML.simpleScenario (flip mkStorage mempty) blndOnTezosContract impl
      niComment impl "Bridge scenario"
      simpleScenario (\alice bob -> mkStorage alice $ fromList [(alice, 1000), (bob, 1000)])
        blndOnTezosContract impl

simpleScenario
  :: forall m storage capsM.
     ( Monad m, capsM ~ NettestT m
     , NiceStorage storage
     )
  => (Address -> Address -> storage)
  -> Contract Parameter storage
  -> NettestImpl m
  -> m ()
simpleScenario mkInitialStorage contract = uncapsNettest $ do
  aliceAddr <- resolveNettestAddress
  bobAddr <- newAddress "bob"
  c <- originateSimple "BlndOnTezos" (mkInitialStorage aliceAddr bobAddr) contract
  let swapId1 = SwapId ("swapId1" :: ByteString)
      swapId2 = SwapId ("swapId2" :: ByteString)
      secret = "secret" :: ByteString
      secretHash = sha256 secret
      alice = AddressResolved aliceAddr
      bob = AddressResolved bobAddr


  comment "Lock"
  callFrom alice c (Call @"Lock") $ LockParams
    { lpId          = swapId1
    , lpTo          = bobAddr
    , lpAmount      = 100
    , lpReleaseTime = maxTimestamp
    , lpSecretHash  = Nothing
    }

  comment "RevealSecretHash"
  callFrom alice c (Call @"RevealSecretHash") $ RevealSecretHashParams
    { rshpId         = swapId1
    , rshpSecretHash = secretHash
    }

  comment "Redeem"
  callFrom bob c (Call @"Redeem") $ RedeemParams
    { rpId     = swapId1
    , rpSecret = secret
    }

  comment "Lock in the past"
  callFrom alice c (Call @"Lock") $ LockParams
    { lpId          = swapId2
    , lpTo          = bobAddr
    , lpAmount      = 100
    , lpReleaseTime = Timestamp 10
    , lpSecretHash  = Nothing
    }

  comment "Refund"
  callFrom alice c (Call @"ClaimRefund") $ ClaimRefundParams swapId2
