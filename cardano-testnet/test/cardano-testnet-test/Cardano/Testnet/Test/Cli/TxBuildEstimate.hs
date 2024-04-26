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
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Stack
import           Lens.Micro
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Testnet.Components.Query
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime
import           Testnet.Start.Types

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
    [ eraToString era, "query", "protocol-parameters", "--out-file", pparamsFp ]

  wallet0KeyHash <- Text.pack . filter (/= '\n') <$> H.execCli' execConfig
                [ "address", "key-hash"
                , "--payment-verification-key-file", paymentVKey $ paymentKeyInfoPair wallet0
                ]

  plutusMintingScript <- H.note $ work </> "always-succeeds-non-spending-script.plutusV3"
  H.writeFile plutusMintingScript $ Text.unpack plutusV3NonSpendingScript

  mintingPolicyId <- filter (/= '\n') <$>
    H.execCli' execConfig
      [ anyEraToString anyEra, "transaction"
      , "policyid"
      , "--script-file", plutusMintingScript
      ]
  let assetName = "6E6F64657465616D"
      mintValue = mconcat ["5 ", mintingPolicyId, ".", assetName]

  void $ execCli' execConfig
      [ eraToString era, "transaction", "build-estimate"
      , "--shelley-key-witnesses", show @Int 1
      , "--protocol-params-file", pparamsFp
      , "--total-utxo-value", onlyAda
      , "--tx-in", Text.unpack $ renderTxIn txin1
      , "--tx-in-collateral", Text.unpack $ renderTxIn txin1
      , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
      , "--mint", mintValue
      , "--mint-script-file", plutusMintingScript
      , "--mint-redeemer-value", "0"
      , "--mint-execution-units", "(2000000,20000000)"
      , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_001
      , "--out-file", txbodyFp
      ]

  cddlUnwitnessedTx <- H.readJsonFileOk txbodyFp
  apiTx <- H.evalEither $ deserialiseTxLedgerCddl sbe cddlUnwitnessedTx
  let txFee = L.unCoin $ H.extractTxFee apiTx

  -- This is the current calculated fee.
  -- It's a sanity check to see if anything has
  -- changed regarding fee calculation.
  1154623 H.=== txFee

  void $ execCli' execConfig
    [ eraToString era, "transaction", "sign"
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp
    ]


  void $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", txbodySignedFp
    ]

  H.threadDelay 5_000_000

  -- Test with MAs in the UTxO
  txin2 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0
  utxos2 <- findUtxosWithAddress epochStateView sbe (paymentKeyInfoAddr wallet0)

  txbodyFp2 <- H.note $ work </> "tx-ma-utxo.body"
  txbodySignedFp2 <- H.note $ work </> "tx-ma-utxo.body.signed"
  onlyAda2 <-
    case Map.lookup txin2 utxos2 of
      Nothing -> H.failMessage callStack $ "Tx input not found: " <> Text.unpack (renderTxIn txin2)
      Just txout -> return . show @Integer . L.unCoin . txOutValueLovelace $  txOutValue txout
  let outputWithSingleMa = mconcat [ Text.unpack (paymentKeyInfoAddr wallet0)
                                   , "+", show @Int 1_034_400
                                   , "+", mintValue
                                   ]
  void $ execCli' execConfig
      [ eraToString era, "transaction", "build-estimate"
      , "--shelley-key-witnesses", show @Int 1
      , "--protocol-params-file", pparamsFp
      , "--total-utxo-value", mconcat [onlyAda2, "+", mintValue]
      , "--tx-in", Text.unpack $ renderTxIn txin2
      , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
      , "--tx-out", outputWithSingleMa
      , "--out-file", txbodyFp2
      ]


  cddlUnwitnessedTx2 <- H.readJsonFileOk txbodyFp2
  apiTx2 <- H.evalEither $ deserialiseTxLedgerCddl sbe cddlUnwitnessedTx2
  let txFee2 = L.unCoin $ H.extractTxFee apiTx2

  -- This is the current calculated fee.
  -- It's a sanity check to see if anything has
  -- changed regarding fee calculation.
  -- Without the multi-asset value the fee is 228.
  278 H.=== txFee2

  void $ execCli' execConfig
    [ eraToString era, "transaction", "sign"
    , "--tx-body-file", txbodyFp2
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair wallet0
    , "--out-file", txbodySignedFp2
    ]


  void $ execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", txbodySignedFp2
    ]

  H.success


txOutValue :: TxOut ctx era -> TxOutValue era
txOutValue (TxOut _ v _ _) = v

txOutValueLovelace ::TxOutValue era -> L.Coin
txOutValueLovelace = \case
  TxOutValueShelleyBased sbe v -> v ^. L.adaAssetL sbe
  TxOutValueByron v -> v

exampleSimpleScript :: Text -> Text
exampleSimpleScript requiredSignerKeyHash =
  Text.unlines ["{"
               , "\"keyHash\": " <> "\"" <> requiredSignerKeyHash <> "\""
               , ",\"type\": \"sig\""
               , "}"
               ]
