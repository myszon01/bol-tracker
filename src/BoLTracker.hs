{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    UnloadFreightParams (..),
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
import Plutus.Script.Utils.V2.Typed.Scripts (tyTxOutData, tyTxOutRefOut)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), check, unless)
import Prelude (Show (..), String)
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
  | Delivered BuiltinByteString
  | Rejected BuiltinByteString
  | Finished
  deriving (Show, Prelude.Eq)

instance Eq BoLTrackerState where
  {-# INLINEABLE (==) #-}
  Loading == Loading = True
  (InTransit loc) == (InTransit loc') =
    loc == loc'
  (InTransitRejected loc) == (InTransitRejected loc') =
    loc == loc'
  (Delivered _) == (Delivered _) = True
  (Rejected _) == (Rejected _) = True
  _ == _ = False

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
  | UnloadFreight BoLSignature BuiltinByteString
  | Claim
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
  (v, InTransit _, UnloadFreight Accept notes) ->
    Just (Constraints.mustBeSignedBy (btpReciver btp), State (Delivered notes) v)
  (v, InTransit _, UnloadFreight Reject notes) ->
    Just (Constraints.mustBeSignedBy (btpReciver btp), State (Rejected notes) v)
  (v, Rejected _, CurrentLocation location) ->
    Just (Constraints.mustBeSignedBy (btpCarrier btp), State (InTransitRejected [location]) v)
  (v, InTransitRejected locations, CurrentLocation location) ->
    Just (Constraints.mustBeSignedBy (btpCarrier btp), State (InTransitRejected $ locations ++ [location]) v)
  (v, InTransitRejected _, UnloadFreight Accept notes) ->
    Just (Constraints.mustBeSignedBy (btpShipper btp), State (Delivered notes) v)
  (_, Delivered _, Claim) ->
    Just (Constraints.mustBeSignedBy (btpCarrier btp), State Finished mempty)
  _ -> Nothing

{-# INLINEABLE isFinal #-}
isFinal :: BoLTrackerState -> Bool
isFinal Finished = True
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
updateLocation btp location = do
  let client = btpClient btp
  m <- mapErrorSM $ getOnChainState client
  case m of
    Nothing -> throwError "Contract not found"
    Just (o, _) -> case tyTxOutData $ tyTxOutRefOut $ ocsTxOutRef o of
      InTransit _ -> do
        logInfo @String "Freight is in transit. Updating current location..."
        void $ mapErrorSM $ runStep client $ CurrentLocation location
      Rejected _ -> do
        logInfo @String "Freight is rejected. Sending back to shipper..."
        void $ mapErrorSM $ runStep client $ CurrentLocation location
      InTransitRejected _ -> do
        logInfo @String "Freight is being returned. Updating current location..."
        void $ mapErrorSM $ runStep client $ CurrentLocation location
      _ -> logInfo @String "Wrong contract state"

unloadFreight :: BoLTrackerParams -> BoLSignature -> BuiltinByteString -> Contract w s Text ()
unloadFreight btp s notes = void $ mapErrorSM $ runStep (btpClient btp) $ UnloadFreight s notes

claimReward :: BoLTrackerParams -> Contract w s Text ()
claimReward btp = void $ mapErrorSM $ runStep (btpClient btp) Claim

data UnloadFreightParams = UnloadFreightParams
  { bolSignature :: BoLSignature,
    notes :: BuiltinByteString
  }
  deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''UnloadFreightParams
PlutusTx.makeLift ''UnloadFreightParams

type BTPStartSchema =
  Endpoint "start contract" StartBTP

type BTPUseSchema =
  Endpoint "finish loading" ()
    .\/ Endpoint "update location" BuiltinByteString
    .\/ Endpoint "unload freight" UnloadFreightParams
    .\/ Endpoint "claim reward" ()

startEndpoint :: Contract (Last BoLTrackerParams) BTPStartSchema Text ()
startEndpoint =
  forever $
    handleError logError $
      awaitPromise $
        endpoint @"start contract" $
          startBTP

useEndpoints' ::
  ( HasEndpoint "finish loading" () s,
    HasEndpoint "update location" BuiltinByteString s,
    HasEndpoint "unload freight" UnloadFreightParams s,
    HasEndpoint "claim reward" () s
  ) =>
  BoLTrackerParams ->
  Promise () s Text ()
useEndpoints' btp = finishLoading' `select` updateLocation' `select` unloadFreight' `select` claimReward'
  where
    finishLoading' = endpoint @"finish loading" $ \() -> handleError logError (finishLoading btp)
    updateLocation' = endpoint @"update location" $ \location -> handleError logError (updateLocation btp location)
    unloadFreight' = endpoint @"unload freight" $ \UnloadFreightParams {bolSignature, notes} -> handleError logError (unloadFreight btp bolSignature notes)
    claimReward' = endpoint @"claim reward" $ \() -> handleError logError (claimReward btp)

useEndpoints :: BoLTrackerParams -> Contract () BTPUseSchema Text ()
useEndpoints = forever . awaitPromise . useEndpoints'