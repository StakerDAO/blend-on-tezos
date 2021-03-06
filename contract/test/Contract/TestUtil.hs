module Contract.TestUtil
  ( OrigParams (..)
  , lookupE
  , shouldBe
  , checkThat
  , getLedger
  , getTotalSupply
  , getSwaps
  , getOutcomes
  ) where

import Prelude

import Lorentz (Address, unBigMap)
import Lorentz.Test (TestError (..))

import Contract.BlndOnTezos (Storage (..))
import Contract.Bridge (BridgeStorage (..), Outcome, Swap, SwapId)
import Contract.Token (LedgerValue, ManagedLedgerStorage (..))
import Data.Map (lookup)

data OrigParams = OrigParams
  { opBalances :: Map Address Natural
  , opAlice    :: Address
  , opBob      :: Address
  } deriving Show

lookupE :: (Show k, Ord k) => k -> Map k b -> Either TestError b
lookupE a m = case lookup a m of
  Just b  -> Right b
  Nothing -> Left $ CustomTestError $ "No such key in map" <> show a

shouldBe :: (Eq a, Show a) => a -> a -> Either Text ()
shouldBe actual expected
  | actual == expected = Right ()
  | otherwise = Left $
      "Expected value:\n" <> show expected <> "\n" <>
      "But actual value is:\n" <> show actual <> "\n"

checkThat :: Text -> Either Text () -> Either TestError ()
checkThat check = \case
  Right _ -> Right ()
  Left err -> Left $ CustomTestError $ "Check: \"" <> check <> "\" failed.\n" <> err <> ""

getLedger :: Storage -> Map Address LedgerValue
getLedger = unBigMap . mlsLedger . sToken

getTotalSupply :: Storage -> Natural
getTotalSupply = mlsTotalSupply . sToken

getSwaps :: Storage -> Map SwapId Swap
getSwaps = unBigMap . sSwaps . sBridge

getOutcomes :: Storage -> Map SwapId Outcome
getOutcomes = unBigMap . sOutcomes . sBridge

