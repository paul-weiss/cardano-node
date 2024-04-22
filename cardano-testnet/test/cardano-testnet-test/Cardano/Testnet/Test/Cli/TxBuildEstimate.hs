{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use head" -}

module Cardano.Testnet.Test.Cli.TxBuildEstimate
  ( hprop_tx_build_estimate
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Ledger.Lens as L

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import           Lens.Micro
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO


hprop_tx_build_estimate :: Property
hprop_tx_build_estimate = H.integrationWorkspace "tx-build-estimate" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    sbe = ShelleyBasedEraConway
    era = toCardanoEra sbe
    anyEra = AnyCardanoEra era
    options = cardanoDefaultTestnetOptions
                        { cardanoNodes = cardanoDefaultTestnetNodeOptions
                        , cardanoSlotLength = 0.1
                        , cardanoNodeEra = anyEra -- TODO: We should only support the latest era and the upcoming era
                        }
  TestnetRuntime
    { configurationFile
    , testnetMagic
    , poolNodes
    , wallets = wallet0:_
    } <- cardanoTestnetDefault options conf

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  epochStateView <- getEpochStateView (File configurationFile) (File socketPath)

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  utxos <- findUtxosWithAddress epochStateView sbe (paymentKeyInfoAddr wallet0)

  pparamsFp <- H.note $ work </> "pparams.json"

  let -- TODO: Test with a multi-asset in the UTxO
      onlyAda = show @Integer $ sum $ map (L.unCoin . txOutValueLovelace . txOutValue) $ Map.elems utxos
  void $ H.execCli' execConfig
    [ "babbage", "query", "protocol-parameters", "--out-file", pparamsFp ]


  void $ execCli' execConfig
      [ "babbage", "transaction", "build-estimate"
      , "--shelley-key-witnesses", show @Int 1
      , "--protocol-params-file", pparamsFp
      , "--total-utxo-value", onlyAda
      , "--tx-in", Text.unpack $ renderTxIn txin1
      , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
      , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_001
      , "--out-file", txbodyFp
      ]

  void $ execCli' execConfig
    [ "babbage", "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp
    ]


  void $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]
  H.success


txOutValue :: TxOut ctx era -> TxOutValue era
txOutValue (TxOut _ v _ _) = v

txOutValueLovelace ::TxOutValue era -> L.Coin
txOutValueLovelace = \case
  TxOutValueShelleyBased sbe v -> v ^. L.adaAssetL sbe
  TxOutValueByron v -> v
