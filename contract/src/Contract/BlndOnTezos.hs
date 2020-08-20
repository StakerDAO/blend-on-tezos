module Contract.BlndOnTezos
  ( blndOnTezosIndigo
  , blndOnTezosContract
  , blndOnTezosDoc
  , mkStorage
  , Parameter
  , Storage
  ) where

import Indigo

import Lorentz (cCode)
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)
import Lorentz.Run (Contract)

import qualified Contract.Bridge as Bridge
import qualified Contract.Token as Token

-- | Top-level parameter of the NBIT contract.
data Parameter
  = Token Token.Parameter
  | Bridge Bridge.Parameter
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdRecursive

-- | Top-level storage of the NBIT contract.
data Storage = Storage
  { sTokenStorage  :: Token.Storage
  , sBridgeStorage :: Bridge.Storage
  }
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Storage where
  typeDocMdDescription = "Managed ledger storage fields."
  typeDocHaskellRep = homomorphicTypeDocHaskellRep
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

mkStorage :: Address -> Map Address Natural -> Storage
mkStorage adminAddress balances = Storage
  { sTokenStorage = Token.mkStorage adminAddress balances
  , sBridgeStorage = Bridge.mkStorage
  }

instance HasField Storage "ledger" (BigMap Address Token.LedgerValue) where
  fieldLens = fieldLensDeeper #sTokenStorage

instance HasField Storage "approvals" (BigMap GetAllowanceParams Natural) where
  fieldLens = fieldLensDeeper #sTokenStorage

instance HasField Storage "admin" Address where
  fieldLens = fieldLensDeeper #sTokenStorage

instance HasField Storage "paused" Bool where
  fieldLens = fieldLensDeeper #sTokenStorage

instance HasField Storage "totalSupply" Natural where
  fieldLens = fieldLensDeeper #sTokenStorage

instance HasField Storage "swaps" (BigMap Bridge.SwapId Bridge.Swap) where
  fieldLens = fieldLensDeeper #sBridgeStorage

instance HasField Storage "outcomes" (BigMap Bridge.SwapId Bridge.Outcome) where
  fieldLens = fieldLensDeeper #sBridgeStorage

blndOnTezosIndigo :: IndigoContract Parameter Storage
blndOnTezosIndigo param = contractName "BLND on Tezos" do
  contractGeneralDefault
  docStorage @Storage
  doc $ DDescription
    "This documentation describes a smart contract which implements FA1.2 interface and coin swap."
  case_ param
    ( #cToken //-> Token.entrypoints @Storage
    , #cBridge //-> Bridge.entrypoints @Storage
    )

blndOnTezosContract :: Contract Parameter Storage
blndOnTezosContract = defaultContract $ compileIndigoContract blndOnTezosIndigo

blndOnTezosDoc :: ContractDoc
blndOnTezosDoc = buildLorentzDocWithGitRev DGitRevisionUnknown $ cCode blndOnTezosContract
