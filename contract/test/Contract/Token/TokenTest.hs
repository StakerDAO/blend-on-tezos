module Contract.Token.TokenTest
  ( spec_Token
  , test_ManagedLedgerIndigoSMT
  ) where

import Prelude

import Lorentz.Contracts.Test.ApprovableLedger (approvableLedgerSpec)
import Lorentz.Contracts.Test.ManagedLedger (managedLedgerSpec)
import Lorentz.Contracts.Test.SMT.ApprovableLedger (approvableLedgerSMT)
import Lorentz.Contracts.Test.SMT.ManagedLedger (managedLedgerSMT)
import Test.Hspec (Spec)
import Test.Tasty (testGroup)
import Test.Tasty.Providers (TestTree)

import Contract.TestSetup (originateContractForManagedLedger)

spec_Token :: Spec
spec_Token = do
  approvableLedgerSpec originateContractForManagedLedger
  managedLedgerSpec originateContractForManagedLedger

test_ManagedLedgerIndigoSMT :: [TestTree]
test_ManagedLedgerIndigoSMT =
  [ testGroup "Approvable ledger state machine tests" $
      approvableLedgerSMT originateContractForManagedLedger
  , testGroup "Managed ledger state machine tests" $
      managedLedgerSMT originateContractForManagedLedger
  ]
