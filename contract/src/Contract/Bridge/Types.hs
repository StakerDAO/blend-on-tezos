module Contract.Bridge.Types
  ( LockParams (..)
  , SwapId (..)
  , Swap (..)
  , Outcome (..)
  , RevealSecretHashParams (..)
  , RedeemParams (..)
  , TooLongSecretError (..)
  , ClaimRefundParams (..)
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
  = Refunded ()
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
   '[ '( "LockParams", '( 'Nothing,
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
   '[ '( "RevealSecretHashParams", '( 'Nothing,
         '[ '("rshpId", "Swap id.")
          , '("rshpSecreteHash", "Hash of the secret.")
          ])
       )
    ]

data RedeemParams = RedeemParams
  { rpId     :: SwapId
  , rpSecret :: ByteString
  } deriving stock Generic
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc RedeemParams where
  typeDocMdDescription = "RevealSecretHash entrypoint params."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "RedeemParams", '( 'Nothing,
         '[ '("rpId", "Swap id.")
          , '("rpSecret", "Secret.")
          ])
       )
    ]

data TooLongSecretError = TooLongSecretError
 { tlseExpected :: Natural
 , tlseActual   :: Natural
 } deriving stock Generic
   deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc TooLongSecretError where
  typeDocMdDescription = "Data for too long secter error."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "TooLongSecretError", '( 'Nothing,
         '[ '("tlseExpected", "Expected lenght limit of the secrete.")
          , '("tlseActual", "Actual lenght of the secrete.")
          ])
       )
    ]

newtype ClaimRefundParams = ClaimRefundParams {crpId :: SwapId}
  deriving stock Generic
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc ClaimRefundParams where
  typeDocMdDescription = "ClaimRefund params."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "ClaimRefundParams", '( 'Nothing,
         '[ '("crpId", "Swap id.")
          ])
       )
    ]
