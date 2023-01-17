{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module TestBoLTracker
  ( runMyTrace,
    TraceType (..),
  )
where

import BoLTracker
import BoLTracker qualified as BT
import Control.Monad hiding (fmap)
import Control.Monad.Freer.Extras as Extras
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import GHC.Generics (Generic)
import Ledger
import Ledger.Ada as Ada
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Builtins.Class
import PlutusTx.Prelude
import Wallet.Emulator qualified as EM
import Prelude (IO, Show (..), String)

data TraceType = AllGood | Rejected
  deriving (Show, Generic, FromJSON, ToJSON)

runContract :: TraceType -> IO ()
runContract AllGood = runEmulatorTraceIO' def emCfg shipFreightAllGood
runContract Rejected = runEmulatorTraceIO' def emCfg shipFreightRejectFullLoad

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1000_000_000

shipFreightAllGood :: EmulatorTrace ()
shipFreightAllGood = do
  h <- activateContractWallet w1 BT.startEndpoint

  let sbtp =
        StartBTP
          { sbtpShipper = EM.mockWalletPaymentPubKeyHash w1,
            sbtpCarrier = EM.mockWalletPaymentPubKeyHash w2,
            sbtpReciver = EM.mockWalletPaymentPubKeyHash w3,
            sbtpBoLURL = stringToBuiltinByteString "url to version contol"
          }

  callEndpoint @"start contract" h sbtp
  void $ Emulator.waitNSlots 5
  Last m <- observableState h
  case m of
    Nothing -> Extras.logError @String "error starting token sale"
    Just btp -> do
      Extras.logInfo $ "started loading freight" ++ show btp

      h1 <- activateContractWallet w1 $ useEndpoints btp
      h2 <- activateContractWallet w2 $ useEndpoints btp
      h3 <- activateContractWallet w3 $ useEndpoints btp

      callEndpoint @"finish loading" h1 ()
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Chicago, IL"
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Denver, CO"
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Portland, OR"
      void $ Emulator.waitNSlots 5

      let ufp =
            UnloadFreightParams
              { bolSignature = Accept,
                notes = stringToBuiltinByteString "All Good"
              }
      callEndpoint @"unload freight" h3 ufp
      void $ Emulator.waitNSlots 5

      callEndpoint @"claim reward" h2 ()
      void $ Emulator.waitNSlots 5

shipFreightRejectFullLoad :: EmulatorTrace ()
shipFreightRejectFullLoad = do
  h <- activateContractWallet w1 BT.startEndpoint

  let sbtp =
        StartBTP
          { sbtpShipper = EM.mockWalletPaymentPubKeyHash w1,
            sbtpCarrier = EM.mockWalletPaymentPubKeyHash w2,
            sbtpReciver = EM.mockWalletPaymentPubKeyHash w3,
            sbtpBoLURL = stringToBuiltinByteString "url to version contol"
          }

  callEndpoint @"start contract" h sbtp
  void $ Emulator.waitNSlots 5
  Last m <- observableState h
  case m of
    Nothing -> Extras.logError @String "error starting token sale"
    Just btp -> do
      Extras.logInfo $ "started loading freight" ++ show btp

      h1 <- activateContractWallet w1 $ useEndpoints btp
      h2 <- activateContractWallet w2 $ useEndpoints btp
      h3 <- activateContractWallet w3 $ useEndpoints btp

      callEndpoint @"finish loading" h1 ()
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Chicago, IL"
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Denver, CO"
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Portland, OR"
      void $ Emulator.waitNSlots 5

      let ufp1 =
            UnloadFreightParams
              { bolSignature = Reject,
                notes = stringToBuiltinByteString "Broken Seal"
              }
      callEndpoint @"unload freight" h3 ufp1
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Denver, CO"
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Chicago, IL"
      void $ Emulator.waitNSlots 5

      let ufp2 =
            UnloadFreightParams
              { bolSignature = Accept,
                notes = stringToBuiltinByteString "Recived returned freight"
              }
      callEndpoint @"unload freight" h1 ufp2
      void $ Emulator.waitNSlots 5

      callEndpoint @"claim reward" h2 ()
      void $ Emulator.waitNSlots 5