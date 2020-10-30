module Contract.ContractCommunication
  ( printContractInformation
  ) where

import Indigo

import Contract.BlndOnTezos (Parameter, Storage, blndOnTezosDoc, blndOnTezosIndigo, mkStorage)
import Tezos.Address (unsafeParseAddress)
import Universum (writeFile)

printContractInformation :: IO ()
printContractInformation = do
  let storage = toStrict $ printLorentzValue True $ mkStorage
        (unsafeParseAddress "tz1b3eGoYhuJ6cPPHAr1SZrYdZGtoYGyxqEW")
        (unsafeParseAddress "tz1b3eGoYhuJ6cPPHAr1SZrYdZGtoYGyxqEW")
        mempty
      documentation = toStrict $ contractDocToMarkdown blndOnTezosDoc
  saveAsMichelson @Parameter @Storage blndOnTezosIndigo "morley-contract/contract.tz"
  writeFile "morley-contract/storage.tz" storage
  writeFile "doc/specification.md" documentation
