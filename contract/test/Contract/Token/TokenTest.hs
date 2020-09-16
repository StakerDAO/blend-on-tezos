module Contract.Token.TokenTest 
  ( spec_Token
  , test_ManagedLedgerIndigoSMT
  ) where
  
import Prelude
  
import Lorentz.Contracts.Test.ApprovableLedger (approvableLedgerSpec)
import Test.Hspec (Spec)
import Test.Tasty.Providers (TestTree)
import Test.Tasty (testGroup)
import Lorentz.Contracts.Test.SMT.ApprovableLedger (approvableLedgerSMT)
import Lorentz.Contracts.Test.SMT.ManagedLedger (managedLedgerSMT)

import Contract.TestSetup (originateContractForManagedLedger)

spec_Token :: Spec
spec_Token = approvableLedgerSpec originateContractForManagedLedger

test_ManagedLedgerIndigoSMT :: [TestTree]
test_ManagedLedgerIndigoSMT =
  [ testGroup "Approvable ledger state machine tests" $
      approvableLedgerSMT originateContractForManagedLedger
  , testGroup "Managed ledger state machine tests" $
      managedLedgerSMT originateContractForManagedLedger
  ]
