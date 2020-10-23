module Contract.Bridge.Types
  ( LockParams (..)
  , SecretHash (..)
  , Swap (..)
  , Outcome (..)
  , ConfirmSwapParams (..)
  , RedeemParams (..)
  , TooLongSecretError (..)
  , ClaimRefundParams (..)
  , GetSwapParams
  , GetOutcomeParams
  ) where

import Indigo

import Fmt (Buildable (..), hexF)

newtype SecretHash = SecretHash { shHash :: ByteString }
  deriving stock (Eq, Ord, Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

type GetSwapParams = View SecretHash (Maybe Swap)
type GetOutcomeParams = View SecretHash (Maybe Outcome)

instance TypeHasDoc SecretHash where
  typeDocMdDescription = "Id of the swap."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep

instance Buildable SecretHash where
  build (SecretHash sId) = hexF sId

data Swap = Swap
  { sFrom        :: Address
  , sTo          :: Address
  , sAmount      :: Natural
  , sReleaseTime :: Timestamp
  , sFee         :: Maybe Natural
  , sSecretHash  :: SecretHash
  , sConfirmed   :: Bool
  } deriving stock (Generic, Show, Eq)
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
          , '("sFee", "Amount of fee that pay initiator of the contract.")
          , '("sSecretHash", "Hash of the swap secrete.")
          , '("sConfirmed", "Condition which say that the initiator confirmed the swap.")
          ])
       )
    ]

instance Buildable Swap where
  build = show

newtype Outcome = Outcome { oSecret :: ByteString }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc Outcome where
  typeDocMdDescription = "Outcome information."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "Outcome", '( 'Nothing,
         '[ '("oSecret", "Resieved secret of the swap.")
          ])
       )
    ]

instance Buildable Outcome where
  build = show

data LockParams = LockParams
  { lpTo          :: Address
  , lpAmount      :: Natural
  , lpReleaseTime :: Timestamp
  , lpSecretHash  :: SecretHash
  , lpFee         :: Maybe Natural
  , lpConfirmed   :: Bool
  } deriving stock (Generic, Show)
    deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc LockParams where
  typeDocMdDescription = "Lock entrypoint params."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "LockParams", '( 'Nothing,
         '[ '("lpTo", "Address of swap reciever.")
          , '("lpAmount", "Number of tokens in swap.")
          , '("lpReleaseTime", "Time for swap process.")
          , '("lpSecretHash", "Hash of the secret.")
          , '("lpFee", "Amount of fee that pay initiator of the contract.")
          , '("lpConfirmed", "Condition which say that the initiator confirmed the swap.")
          ])
       )
    ]

newtype ConfirmSwapParams = ConfirmSwapParams { cspSecretHash :: SecretHash }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc ConfirmSwapParams where
  typeDocMdDescription = "ConfirmSwap entrypoint params."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "ConfirmSwapParams", '( 'Nothing,
         '[ '("cspSecretHash", "Hash of the secret.")
          ])
       )
    ]

newtype RedeemParams = RedeemParams { rpSecret :: ByteString }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc RedeemParams where
  typeDocMdDescription = "Redeem entrypoint params."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "RedeemParams", '( 'Nothing,
         '[ '("rpSecret", "Secret.")
          ])
       )
    ]

newtype ClaimRefundParams = ClaimRefundParams {crpSecretHash :: SecretHash}
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc ClaimRefundParams where
  typeDocMdDescription = "ClaimRefund params."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "ClaimRefundParams", '( 'Nothing,
         '[ '("crpSecretHash", "Swap id.")
          ])
       )
    ]

data TooLongSecretError = TooLongSecretError
 { tlseExpected :: Natural
 , tlseActual   :: Natural
 } deriving stock (Generic, Show, Eq)
   deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc TooLongSecretError where
  typeDocMdDescription = "Data for too long secter error."
  typeDocMichelsonRep = homomorphicTypeDocMichelsonRep
  type TypeDocFieldDescriptions _ =
   '[ '( "TooLongSecretError", '( 'Nothing,
         '[ '("tlseExpected", "Expected lenght limit of the secret.")
          , '("tlseActual", "Actual lenght of the secret.")
          ])
       )
    ]
