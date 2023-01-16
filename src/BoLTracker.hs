{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module BoLTracker () where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..))
import qualified Prelude
import Data.Bool (Bool(True))
import Language.Haskell.TH (location)

-- ON CHAIN --

data BoLTrackerParams = BoLTrackerParams
    { btpShipper :: !PaymentPubKeyHash
    , btpReciver :: !PaymentPubKeyHash
    , btpCarrier :: !PaymentPubKeyHash
    , btpBoLURL  :: !BuiltinByteString
    , btpTT     :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''BoLTrackerParams

data BoLTrackerState =
      Loading
    | Shipped 
    | InTransit [BuiltinByteString]
    | Unload
    | Delivered
    | Rejected
    deriving (Show, Prelude.Eq)

instance Eq BoLTrackerState where
    {-# INLINABLE (==) #-}
    Loading == Loading = True
    Shipped == Shipped = True
    (InTransit loc) == (InTransit loc' pks') =
        loc == loc'
    Delivered == Delivered = True
    Rejected == Rejected = True
    _ == _ = False

PlutusTx.unstableMakeIsData ''BoLTrackerState
PlutusTx.makeLift ''BoLTrackerState

data BoLSignature = Accepted | Rejected
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq BoLSignature where
    {-# INLINABLE (==) #-}
    Accepted == Accepted = True
    Rejected == Rejected  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''BoLSignature

data BoLTrackerActions =
    FinishLoading
    | SignBol BoLSignature (Maybe BuiltinByteString)
    | CurrentLocation BuiltinByteString
    | UnloadFreight
    deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''BoLTrackerActions

{-# INLINABLE transition #-}
transition :: BoLTrackerParams -> State BoLTrackerState -> BoLTrackerActions -> Maybe (TxConstraints Void Void, State BoLTrackerState)
transition btp s r = case (stateValue s, stateData s, r) of
    (v, Loading, FinishLoading) -> 
        Just ( Constraints.mustBeSignedBy (btpShipper btp), State (InTransit []) v)
    (v, InTransit locations, CurrentLocation location)  -> 
        Just ( Constraints.mustBeSignedBy (btpCarrier btp), State (InTransit $ locations : location) v)
    _   ->  Nothing


{-# INLINABLE isFinal #-}
isFinal :: BoLTrackerState -> Bool
isFinal Delivered = True
isFinal _        = False

{-# INLINABLE btpStateMachine #-}
btpStateMachine :: BoLTrackerParams -> StateMachine BoLTrackerState BoLTrackerActions
btpStateMachine btp = mkStateMachine (Just $ btpTT btp) (transition btp) isFinal

{-# INLINABLE mkBTPValidator #-}
mkBTPValidator :: BoLTrackerParams -> BoLTrackerState -> BoLTrackerActions -> ScriptContext -> Bool
mkBTPValidator = mkValidator . btpStateMachine

type BTP = StateMachine BoLTrackerState BoLTrackerActions

btpTypedValidator :: BoLTrackerParams -> Scripts.TypedValidator BTP
btpTypedValidator = Scripts.mkTypedValidatorParam @BTP
    $$(PlutusTx.compile [|| mkBTPValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator


btpClient :: BoLTrackerParams -> StateMachineClient BoLTrackerState BoLTrackerActions
btpClient btp = mkStateMachineClient $ StateMachineInstance (btpStateMachine btp) (btpTypedValidator btp)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show


-- OFF CHAIN --
data StartBTP = StartBTP
    { btpShipper :: !PaymentPubKeyHash
    , btpReciver :: !PaymentPubKeyHash
    , btpCarrier :: !PaymentPubKeyHash
    , btpBoLURL  :: !BuiltinByteString
    } deriving (Show, Generic, FromJSON, ToJSON)

startBTP :: StartBTP -> Contract (Last TokenSale) s Text ()
startBTP sbtp = do
    tt  <- mapErrorSM getThreadToken
    let btp = BoLTrackerParams
            { btpShipper = btpShipper sbtp
            , btpReciver = btpReciver sbtp
            , btpCarrier = btpCarrier sbtp
            , btpBoLURL = btpBoLURL sbtp
            , tsTT     = tt
            }
        client = btpClient btp
    void $ mapErrorSM $ runInitialise client Loading mempty
    tell $ Last $ Just btp
    logInfo $ "started loading freight" ++ show btp

finishLoading :: BoLTrackerParams -> Contract w s Text ()
finishLoading btp = void $ mapErrorSM $ runStep (btpClient btp) FinishLoading

updateLocation :: BoLTrackerParams -> BuiltinByteString -> Contract w s Text ()
updateLocation btp location = void $ mapErrorSM $ runStep (btpClient btp) $ CurrentLocation location