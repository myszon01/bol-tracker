{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BoLTracker
  ( BoLTrackerParams (..),
    BoLSignature (..),
    BoLTrackerActions (..),
    StartBTP (..),
    startEndpoint,
    useEndpoints,
    useEndpoints',
    btpClient,
  )
where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Contract
import Plutus.Contract.StateMachine
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), check, unless)
import Prelude (Show (..))
import Prelude qualified

-- ON CHAIN --
-- Cant use ! (https://cardano.stackexchange.com/questions/9124/ghc-error-when-adding-tokenname-to-nftmint-params)
data BoLTrackerParams = BoLTrackerParams
  { btpShipper :: PaymentPubKeyHash,
    btpCarrier :: PaymentPubKeyHash,
    btpReciver :: PaymentPubKeyHash,
    btpBoLURL :: BuiltinByteString,
    btpTT :: ThreadToken
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''BoLTrackerParams
PlutusTx.makeLift ''BoLTrackerParams

data BoLTrackerState
  = Loading
  | InTransit [BuiltinByteString]
  | InTransitRejected [BuiltinByteString]
  | Unloaded
  | Delivered (Maybe BuiltinByteString)
  | Rejected (Maybe BuiltinByteString)
  deriving (Show, Prelude.Eq)

-- instance Eq BoLTrackerState where
--   {-# INLINEABLE (==) #-}
--   Loading == Loading = True
--   (InTransit loc) == (InTransit loc') =
--     loc == loc'
--   Delivered == Delivered = True
--   Rejected == Rejected = True
--   _ == _ = False

PlutusTx.unstableMakeIsData ''BoLTrackerState
PlutusTx.makeLift ''BoLTrackerState

data BoLSignature = Accept | Reject
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

instance Eq BoLSignature where
  {-# INLINEABLE (==) #-}
  Accept == Accept = True
  Reject == Reject = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''BoLSignature
PlutusTx.makeLift ''BoLSignature

data BoLTrackerActions
  = FinishLoading
  | CurrentLocation BuiltinByteString
  | UnloadFreight BoLSignature (Maybe BuiltinByteString)
  deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''BoLTrackerActions
PlutusTx.makeLift ''BoLTrackerActions

{-# INLINEABLE transition #-}
transition :: BoLTrackerParams -> State BoLTrackerState -> BoLTrackerActions -> Maybe (TxConstraints Void Void, State BoLTrackerState)
transition btp s r = case (stateValue s, stateData s, r) of
  (v, Loading, FinishLoading) ->
    Just (Constraints.mustBeSignedBy (btpShipper btp), State (InTransit []) v)
  (v, InTransit locations, CurrentLocation location) ->
    Just (Constraints.mustBeSignedBy (btpCarrier btp), State (InTransit $ locations ++ [location]) v)
  (v, InTransit _, UnloadFreight Accept maybeNotes) ->
    Just (Constraints.mustBeSignedBy (btpReciver btp), State (Delivered maybeNotes) v)
  (v, InTransit _, UnloadFreight Reject maybeNotes) ->
    Just (Constraints.mustBeSignedBy (btpReciver btp), State (Rejected maybeNotes) v)
  (v, Rejected _, CurrentLocation location) ->
    Just (Constraints.mustBeSignedBy (btpCarrier btp), State (InTransitRejected [location]) v)
  (v, InTransitRejected locations, CurrentLocation location) ->
    Just (Constraints.mustBeSignedBy (btpCarrier btp), State (InTransitRejected $ locations ++ [location]) v)
  (v, InTransitRejected _, UnloadFreight Accept maybeNotes) ->
    Just (Constraints.mustBeSignedBy (btpCarrier btp), State (Delivered maybeNotes) v)
  _ -> Nothing

{-# INLINEABLE isFinal #-}
isFinal :: BoLTrackerState -> Bool
isFinal (Delivered _) = True
isFinal _ = False

{-# INLINEABLE btpStateMachine #-}
btpStateMachine :: BoLTrackerParams -> StateMachine BoLTrackerState BoLTrackerActions
btpStateMachine btp = mkStateMachine (Just $ btpTT btp) (transition btp) isFinal

{-# INLINEABLE mkBTPValidator #-}
mkBTPValidator :: BoLTrackerParams -> BoLTrackerState -> BoLTrackerActions -> ScriptContext -> Bool
mkBTPValidator = mkValidator . btpStateMachine

type BTP = StateMachine BoLTrackerState BoLTrackerActions

btpTypedValidator :: BoLTrackerParams -> Scripts.TypedValidator BTP
btpTypedValidator =
  Scripts.mkTypedValidatorParam @BTP
    $$(PlutusTx.compile [||mkBTPValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator

btpClient :: BoLTrackerParams -> StateMachineClient BoLTrackerState BoLTrackerActions
btpClient btp = mkStateMachineClient $ StateMachineInstance (btpStateMachine btp) (btpTypedValidator btp)

mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

-- OFF CHAIN --
data StartBTP = StartBTP
  { sbtpShipper :: !PaymentPubKeyHash,
    sbtpReciver :: !PaymentPubKeyHash,
    sbtpCarrier :: !PaymentPubKeyHash,
    sbtpBoLURL :: !BuiltinByteString
  }
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''StartBTP
PlutusTx.makeLift ''StartBTP

startBTP :: StartBTP -> Contract (Last BoLTrackerParams) s Text ()
startBTP sbtp = do
  tt <- mapErrorSM getThreadToken
  let btp =
        BoLTrackerParams
          { btpShipper = sbtpShipper sbtp,
            btpCarrier = sbtpCarrier sbtp,
            btpReciver = sbtpReciver sbtp,
            btpBoLURL = sbtpBoLURL sbtp,
            btpTT = tt
          }
      client = btpClient btp
  void $ mapErrorSM $ runInitialise client Loading mempty
  tell $ Last $ Just btp
  logInfo $ "started loading freight" ++ show btp

finishLoading :: BoLTrackerParams -> Contract w s Text ()
finishLoading btp = void $ mapErrorSM $ runStep (btpClient btp) FinishLoading

updateLocation :: BoLTrackerParams -> BuiltinByteString -> Contract w s Text ()
updateLocation btp location = void $ mapErrorSM $ runStep (btpClient btp) $ CurrentLocation location

type BTPStartSchema =
  Endpoint "start contract" StartBTP

type BTPUseSchema =
  Endpoint "finish loading" ()
    .\/ Endpoint "update location" BuiltinByteString

startEndpoint :: Contract (Last BoLTrackerParams) BTPStartSchema Text ()
startEndpoint =
  forever $
    handleError logError $
      awaitPromise $
        endpoint @"start contract" $
          startBTP

useEndpoints' ::
  ( HasEndpoint "finish loading" () s,
    HasEndpoint "update location" BuiltinByteString s
  ) =>
  BoLTrackerParams ->
  Promise () s Text ()
useEndpoints' btp = finishLoading' `select` updateLocation'
  where
    finishLoading' = endpoint @"finish loading" $ \() -> handleError logError (finishLoading btp)
    updateLocation' = endpoint @"update location" $ \location -> handleError logError (updateLocation btp location)

useEndpoints :: BoLTrackerParams -> Contract () BTPUseSchema Text ()
useEndpoints = forever . awaitPromise . useEndpoints'