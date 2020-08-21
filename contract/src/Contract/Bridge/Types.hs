module Contract.Bridge.Types
  ( LockParams (..)
  , SwapId (..)
  , Swap (..)
  , Outcome (..)
  , RevealSecretHashParams (..)
  ) where

import Indigo

newtype SwapId = SwapId ByteString
  deriving stock (Eq, Ord, Generic)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc SwapId where
  typeDocMdDescription = "Id of the swap."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

data Swap = Swap
  { sFrom        :: Address
  , sTo          :: Address
  , sAmount      :: Natural
  , sReleaseTime :: Timestamp
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Swap where
  typeDocMdDescription = "Swap information."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "Swap", '( 'Nothing,
         '[ '("sFrom", "Address of swap initiator.")
          , '("sTo", "Address of swap reciever.")
          , '("sAmount", "Number of tokens in swap.")
          , '("sReleaseTime", "Time for swap process.")
          ])
       )
    ]

data Outcome
  = Refunded
  | HashRevealed ByteString
  | SecretRevealed ByteString
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Outcome where
  typeDocMdDescription = "Outcome storage fields."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
     '[ '("Refunded", '( 'Just "Swap was refunded", '[]))
      , '("HashRevealed", '( 'Just "Secret hash was revealed", '[]))
      , '("SecretRevealed", '( 'Just "Secret was revealed", '[]))
      ]

data LockParams = LockParams
  { lpId          :: SwapId
  , lpTo          :: Address
  , lpAmount      :: Natural
  , lpReleaseTime :: Timestamp
  , lpSecretHash  :: Maybe ByteString
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc LockParams where
  typeDocMdDescription = "Lock entrypoint params."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "LockParams", '( 'Just "Swap was refunded", 
         '[ '("lpId", "Swap id.")
          , '("lpTo", "Address of swap reciever.")
          , '("lpAmount", "Number of tokens in swap.")
          , '("lpReleaseTime", "Time for swap process.")
          , '("lpSecretHash", "Hash of the secret.")
          ])
       )
    ]

data RevealSecretHashParams = RevealSecretHashParams
  { rshpId          :: SwapId
  , rshpSecreteHash :: ByteString
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc RevealSecretHashParams where
  typeDocMdDescription = "RevealSecretHash entrypoint params."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "RevealSecretHashParams", '( 'Just "Swap was refunded", 
         '[ '("rshpId", "Swap id.")
          , '("rshpSecreteHash", "Hash of the secret.")
          ])
       )
    ]

