module Contract.TestSetup
  ( withContract
  , integrationalTestContract
  , withBridgeContractP
  , originateBlndOnTezos
  , originateContractForManagedLedger
  ) where

import Prelude

import Data.Map (fromList)
import Hedgehog (MonadTest, Property, PropertyT, TestLimit, forAll, property, withTests)
import Hedgehog.Gen.Tezos.Core (midTimestamp)
import Lorentz (Address, TAddress, toMutez)
import Lorentz.Contracts.Test.ApprovableLedger (AlSettings (..))
import Lorentz.Test (IntegrationalScenarioM, integrationalTestProp, lOriginate)
import Michelson.Test.Integrational (setNow)

import Contract.BlndOnTezos (Parameter, blndOnTezosContract, mkStorage)
import Contract.Gen (genOrigParams)
import Contract.TestUtil (OrigParams (..))

originateBlndOnTezos :: OrigParams -> IntegrationalScenarioM (TAddress Parameter)
originateBlndOnTezos OrigParams{..} = lOriginate blndOnTezosContract "Blnd on Tezos"
  (mkStorage opAlice opBalances) $ toMutez 1000

withContract
  :: Monad m
  => (IntegrationalScenarioM (TAddress Parameter) -> OrigParams -> PropertyT m b)
  -> PropertyT m b
withContract scenario = do
  params <- forAll genOrigParams
  let contractM = originateBlndOnTezos params
  scenario contractM params

integrationalTestContract
  :: MonadTest m
  => IntegrationalScenarioM t
  -> (t -> IntegrationalScenarioM ()) -> m ()
integrationalTestContract contractM test = integrationalTestProp $ contractM >>= \c -> do
  setNow midTimestamp
  test c

withBridgeContractP
  :: TestLimit
  -> (IntegrationalScenarioM (TAddress Parameter) -> OrigParams -> PropertyT IO ())
  -> Property
withBridgeContractP testsNumber = withTests testsNumber . property . withContract

originateContractForManagedLedger
  :: Address
  -> AlSettings
  -> IntegrationalScenarioM (TAddress Parameter)
originateContractForManagedLedger addr (AlInitAddresses balances) =
  originateBlndOnTezos $ OrigParams (fromList balances) addr addr
