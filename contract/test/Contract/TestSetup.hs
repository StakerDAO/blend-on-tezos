module Contract.TestSetup
  ( withContract
  , integrationalTestContract
  , withBridgeContractP
  ) where

import Prelude

import Hedgehog (MonadTest, Property, PropertyT, TestLimit, forAll, property, withTests)
import Lorentz (TAddress, toMutez)
import Lorentz.Test (IntegrationalScenarioM, integrationalTestProp, lOriginate)

import Contract.BlndOnTezos (Parameter, blndOnTezosContract, mkStorage)
import Contract.Gen (genOrigParams)
import Contract.TestUtil (OrigParams (..))

withContract
  :: Monad m
  => (IntegrationalScenarioM (TAddress Parameter) -> OrigParams -> PropertyT m b)
  -> PropertyT m b
withContract scenario = do
  params@OrigParams{..} <- forAll genOrigParams
  let contractM = lOriginate blndOnTezosContract "Blnd on Tezos"
        (mkStorage opAlice opBalances) $ toMutez 0
  scenario contractM params

integrationalTestContract
  :: MonadTest m
  => IntegrationalScenarioM t
  -> (t -> IntegrationalScenarioM ()) -> m ()
integrationalTestContract contractM test = integrationalTestProp $ contractM >>= \c -> test c

withBridgeContractP
  :: TestLimit
  -> (IntegrationalScenarioM (TAddress Parameter) -> OrigParams -> PropertyT IO ())
  -> Property
withBridgeContractP testsNumber = withTests testsNumber . property . withContract
