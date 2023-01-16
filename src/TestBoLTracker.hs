{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module TestBoLTracker
  ( runMyTrace,
  )
where

import BoLTracker
import BoLTracker qualified as BT
import Control.Monad hiding (fmap)
import Control.Monad.Freer.Extras as Extras
import Data.Default (Default (..))
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Ledger
import Ledger.Ada as Ada
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Builtins.Class
import PlutusTx.Prelude
import Wallet.Emulator qualified as EM
import Prelude (IO, Show (..), String)

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(knownWallet w, v) | w <- [1 .. 3]]) def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1000_000_000

myTrace :: EmulatorTrace ()
myTrace = do
  h <- activateContractWallet w1 BT.startEndpoint

  let sbtp =
        StartBTP
          { sbtpShipper = EM.mockWalletPaymentPubKeyHash w1,
            sbtpCarrier = EM.mockWalletPaymentPubKeyHash w2,
            sbtpReciver = EM.mockWalletPaymentPubKeyHash w3,
            sbtpBoLURL = stringToBuiltinByteString "a"
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
      -- h3 <- activateContractWallet w3 $ useEndpoints btp

      callEndpoint @"finish loading" h1 ()
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Chicago, IL"
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Denver, CO"
      void $ Emulator.waitNSlots 5

      callEndpoint @"update location" h2 $ stringToBuiltinByteString "Portland, OR"
      void $ Emulator.waitNSlots 5
