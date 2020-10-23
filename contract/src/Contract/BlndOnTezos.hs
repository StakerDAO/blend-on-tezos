module Contract.BlndOnTezos
  ( blndOnTezosIndigo
  , blndOnTezosContract
  , blndOnTezosDoc
  , mkStorage
  , Parameter (..)
  , Storage (..)
  ) where

import Indigo

import Lorentz (cCode)
import Lorentz.Contracts.Spec.ApprovableLedgerInterface (GetAllowanceParams)
import Lorentz.Run (Contract)

import qualified Contract.Bridge as Bridge
import qualified Contract.Token as Token

data Parameter
  = Token Token.Parameter
  | Bridge Bridge.Parameter
  deriving stock Generic
  deriving anyclass IsoValue

instance ParameterHasEntrypoints Parameter where
  type ParameterEntrypointsDerivation Parameter = EpdRecursive

data Storage = Storage
  { sToken  :: Token.ManagedLedgerStorage
  , sBridge :: Bridge.BridgeStorage
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Storage where
  typeDocMdDescription =
    "Storage of the contract. \
    \It is splitted on two parts, one for `Token` storage and one for `Bridge` storage."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "Storage", '( 'Nothing,
         '[ '("sToken", "Managed ledger connected storage.")
          , '("sBridge", "Bridge connected storage.")
          ])
       )
    ]

mkStorage :: Address -> Map Address Natural -> Storage
mkStorage adminAddress balances = Storage
  { sToken = Token.mkStorage adminAddress balances
  , sBridge = Bridge.mkStorage
  }

instance HasField Storage "ledger" (BigMap Address Token.LedgerValue) where
  fieldLens = fieldLensDeeper #sToken

instance HasField Storage "approvals" (BigMap GetAllowanceParams Natural) where
  fieldLens = fieldLensDeeper #sToken

instance HasField Storage "admin" Address where
  fieldLens = fieldLensDeeper #sToken

instance HasField Storage "paused" Bool where
  fieldLens = fieldLensDeeper #sToken

instance HasField Storage "totalSupply" Natural where
  fieldLens = fieldLensDeeper #sToken

instance HasField Storage "swaps" (BigMap Bridge.SecretHash Bridge.Swap) where
  fieldLens = fieldLensDeeper #sBridge

instance HasField Storage "outcomes" (BigMap Bridge.SecretHash Bridge.Outcome) where
  fieldLens = fieldLensDeeper #sBridge

blndOnTezosIndigo :: IndigoContract Parameter Storage
blndOnTezosIndigo param = contractName "BLND on Tezos" do
  contractGeneralDefault
  docStorage @Storage
  doc $ DDescription
    "This documentation describes a smart contract which implements \
    \FA1.2 interface and network bridge."
  case_ param
    ( #cToken #= Token.entrypoints @Storage
    , #cBridge #= Bridge.entrypoints @Storage
    )

blndOnTezosContract :: Contract Parameter Storage
blndOnTezosContract = defaultContract $ compileIndigoContract blndOnTezosIndigo

blndOnTezosDoc :: ContractDoc
blndOnTezosDoc = buildLorentzDocWithGitRev DGitRevisionUnknown $ cCode blndOnTezosContract
