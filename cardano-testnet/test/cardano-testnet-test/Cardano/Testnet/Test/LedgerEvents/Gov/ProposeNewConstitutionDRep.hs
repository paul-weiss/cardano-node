{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitutionDRep
  ( hprop_propose_new_constitution_ratio_enough_delegation
  , hprop_propose_new_constitution_ratio_not_enough_delegation
  ) where

import           Testnet.Defaults as Defaults

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.Testnet

import           Prelude

import qualified Cardano.Ledger.Conway.Governance as Ledger
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Except.Extra
import qualified Data.Map.Strict as Map
import           Data.String
import qualified Data.Text as Text
import           Data.Word
import           GHC.IO.Exception (IOException)
import           GHC.Stack (HasCallStack, callStack)
import           System.FilePath ((</>))

import           Hedgehog
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H

import           Cardano.Api.Ledger (DRepVotingThresholds (..))
import           Cardano.Ledger.BaseTypes (UnitInterval, boundRational)
import           Cardano.Ledger.Conway.Genesis (ConwayGenesis (..))
import           Cardano.Ledger.Conway.PParams (UpgradeConwayPParams (..), ucppDRepVotingThresholds)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import           Data.Functor.Identity (Identity)
import           Data.Maybe (fromJust)
import           Data.Ratio ((%))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Hedgehog as H
import qualified Testnet.Aeson
import qualified Testnet.Aeson as Aeson
import           Testnet.Components.SPO
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime hiding (getStartTime, shelleyGenesis)

import qualified Cardano.Testnet as Cardano
import           Data.Bifunctor (Bifunctor (first))
import Lens.Micro ((^.))

newtype AdditionalCatcher
  = IOE IOException
  deriving Show

-- | Whether the constitution proposition is expected to pass or not in the test
data ExpectedResult =
  -- | Constitution is being changed
  ConstitutionChanged
  -- | Constitution is not changed
  | ConstitutionUnchanged

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/ProposeNewConstitutionDRepRatioNotEnoughDelegation/"'@
hprop_propose_new_constitution_ratio_enough_delegation :: Property
hprop_propose_new_constitution_ratio_enough_delegation =
  hprop_propose_new_constitution ConstitutionChanged $ fromJust $ boundRational @UnitInterval (67 % 100)

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/ProposeNewConstitutionDRepRatioNotEnoughDelegation/"'@
hprop_propose_new_constitution_ratio_not_enough_delegation :: Property
hprop_propose_new_constitution_ratio_not_enough_delegation =
  hprop_propose_new_constitution ConstitutionUnchanged $ fromJust $ boundRational @UnitInterval (100 % 100)

hprop_propose_new_constitution :: ()
  => ExpectedResult
  -> UnitInterval -- ^ The ratio required to change the constitution
  -> Property
hprop_propose_new_constitution expectation dvtUpdateConstitutionRatio = H.integrationRetryWorkspace 2 "propose-new-constitution" $ \tempAbsBasePath' -> do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let
      pvtFun :: DRepVotingThresholds -> DRepVotingThresholds
      pvtFun d = d { dvtUpdateToConstitution = dvtUpdateConstitutionRatio }
      ucppFun :: UpgradeConwayPParams Identity -> UpgradeConwayPParams Identity
      ucppFun u@UpgradeConwayPParams { ucppDRepVotingThresholds } = u { ucppDRepVotingThresholds = pvtFun ucppDRepVotingThresholds }
      rewriteConway g@ConwayGenesis { cgUpgradePParams } = g { cgUpgradePParams = ucppFun cgUpgradePParams }
      conwayGenesis = rewriteConway Defaults.defaultConwayGenesis

  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let sbe = ShelleyBasedEraConway
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      cardanoEpochLength = 100 -- in milliseconds
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength
        , cardanoSlotLength = 0.1
        , cardanoNodeEra = cEra
        }

  startTime <- Cardano.getStartTime
  let shelleyGenesis = Defaults.defaultShelleyGenesis startTime fastTestnetOptions
  alonzoGenesis <- H.evalEither $ first prettyError defaultAlonzoGenesis

  testnetRuntime@TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets
    }
    <- cardanoTestnet fastTestnetOptions conf startTime shelleyGenesis alonzoGenesis conwayGenesis

  poolNode1 <- H.headM poolNodes

  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1

  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath
  H.note_ $ "Foldblocks config file: " <> configurationFile testnetRuntime

  -- Create Conway constitution
  gov <- H.createDirectoryIfMissing $ work </> "governance"
  proposalAnchorFile <- H.note $ work </> gov </> "sample-proposFal-anchor"
  consitutionFile <- H.note $ work </> gov </> "sample-constitution"
  constitutionActionFp <- H.note $ work </> gov </> "constitution.action"

  H.writeFile proposalAnchorFile "dummy anchor data"
  H.writeFile consitutionFile "dummy constitution data"
  constitutionHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data"
    , "--file-text", consitutionFile
    ]

  proposalAnchorDataHash <- H.execCli' execConfig
    [ "conway", "governance"
    , "hash", "anchor-data"
    , "--file-text", proposalAnchorFile
    ]

  let stakeVkeyFp = gov </> "stake.vkey"
      stakeSKeyFp = gov </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }

  let drepVkeyFp :: Int -> FilePath
      drepVkeyFp n = tempAbsPath' </> "drep-keys" </> ("drep" <> show n) </> "drep.vkey"

      drepSKeyFp :: Int -> FilePath
      drepSKeyFp n = tempAbsPath' </> "drep-keys" </> ("drep" <> show n) </> "drep.skey"

      stakeNativeVKeyFp :: Int -> FilePath
      stakeNativeVKeyFp n = tempAbsBasePath' </> "stake-delegators" </> ("delegator" <> show n) </> "staking.vkey"

      stakeNativeSKeyFp :: Int -> FilePath
      stakeNativeSKeyFp n = tempAbsBasePath' </> "stake-delegators" </> ("delegator" <> show n) </> "staking.skey"

      drepCertDelegationFile :: Int -> FilePath
      drepCertDelegationFile n = gov </> "drep-keys" <> "drep" <> show n <> ".delegationcert"

  -- Create Drep registration certificates
  let drepCertFile :: Int -> FilePath
      drepCertFile n = gov </> "drep-keys" <>"drep" <> show n <> ".regcert"
  forM_ [1..3] $ \n -> do
    H.execCli' execConfig
       [ "conway", "governance", "drep", "registration-certificate"
       , "--drep-verification-key-file", drepVkeyFp n
       , "--key-reg-deposit-amt", show @Int 0
       , "--out-file", drepCertFile n
       ]

  -- Retrieve UTxOs for registration submission
  H.note_ =<< H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  txin1 <- do
    utxoJson <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
    UTxO utxo <- H.noteShowM $ decodeEraUTxO sbe utxoJson
    H.noteShow =<< H.headM (Map.keys utxo)
  -- Done obtaining UTxOs

  drepRegTxbodyFp <- H.note $ work </> "drep.registration.txbody"
  drepRegTxSignedFp <- H.note $ work </> "drep.registration.tx"

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--certificate-file", drepCertFile 1
    , "--certificate-file", drepCertFile 2
    , "--certificate-file", drepCertFile 3
    , "--witness-override", show @Int 4
    , "--out-file", drepRegTxbodyFp
    ]

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", drepRegTxbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ head wallets
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", drepRegTxSignedFp
    ]

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", drepRegTxSignedFp
    ]

  H.threadDelay 3_000_000

  -- Delegate votes to the DReps: staking key n delegates to DRep n
  forM_ [1..3] $ \n -> do
    H.execCli' execConfig
       [ "conway", "stake-address", "vote-delegation-certificate"
       , "--stake-verification-key-file", stakeNativeVKeyFp n
       , "--drep-verification-key-file", drepVkeyFp n
       , "--out-file", drepCertDelegationFile n
       ]

  voteDelegTxBodyFp <- H.note $ work </> gov </> "vote_deleg.txbody"
  voteDelegTxFp <- H.note $ work </> gov </> "vote_deleg.tx"

  -- Retrieve UTxOs for certificate submission
  H.note_ =<< H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  txin2 <- do
    utxoJson <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
    UTxO utxo <- H.noteShowM $ decodeEraUTxO sbe utxoJson
    H.noteShow =<< H.headM (Map.keys utxo)
  -- Done obtaining UTxOs

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-in", Text.unpack $ renderTxIn txin2
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--certificate-file", drepCertDelegationFile 1
    , "--certificate-file", drepCertDelegationFile 2
    , "--certificate-file", drepCertDelegationFile 3
    , "--witness-override", show @Int 4 -- To compute the transaction fees correctly
    , "--out-file", voteDelegTxBodyFp
    ]

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", voteDelegTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 1
    , "--signing-key-file", stakeNativeSKeyFp 1
    , "--signing-key-file", stakeNativeSKeyFp 2
    , "--signing-key-file", stakeNativeSKeyFp 3
    , "--out-file", voteDelegTxFp
    ]

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", voteDelegTxFp
    ]

  H.threadDelay 3_000_000

  H.note_ =<< H.execCli' execConfig
    [ "conway", "query", "drep-state"
    , "--testnet-magic", show @Int testnetMagic
    , "--all-dreps"
    ]

  H.note_ =<< H.execCli' execConfig
    [ "conway", "query", "drep-stake-distribution"
    , "--all-dreps"
    , "--testnet-magic", show @Int testnetMagic
    ]
  -- Create constitution proposal

  H.note_ =<< H.execCli' execConfig
    [ "conway", "governance", "action", "create-constitution"
    , "--testnet"
    , "--governance-action-deposit", show @Int 0 -- TODO: Get this from the node
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--constitution-url", "https://tinyurl.com/2pahcy6z"
    , "--constitution-hash", constitutionHash
    , "--out-file", constitutionActionFp
    ]

  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  -- Retrieve UTxOs
  H.note_ =<< H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-3.json"
    ]

  txin3 <- do
    utxoJson <- H.leftFailM . H.readJsonFile $ work </> "utxo-3.json"
    H.noteShow_ utxoJson
    UTxO utxo <- H.noteShowM $ decodeEraUTxO sbe utxoJson
    H.noteShow =<< H.headM (Map.keys utxo)
  -- Done obtaining UTxOs

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-in", Text.unpack $ renderTxIn txin3
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--proposal-file", constitutionActionFp
    , "--out-file", txbodyFp
    ]

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", txbodyFp
    -- , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ head wallets
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 1
    , "--out-file", txbodySignedFp
    ]

  H.noteM_ $ H.execCli' execConfig
    [ "conway", "query", "utxo", "--whole-utxo", "--testnet-magic", "42"]
  -- │ Command: /home/churlin/dev/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-cli-8.19.0.0/x/cardano-cli/build/cardano-cli/cardano-cli conway query utxo --whole-utxo --testnet-magic 42
  -- ┃     │                            TxHash                                 TxIx        Amount
  -- ┃     │ --------------------------------------------------------------------------------------
  -- ┃     │ 25732dd74a0311e12fc51b280ddb7e0dc1899cea5415b76ab57ee454e97b3606     0        2666666666666667 lovelace + TxOutDatumNone
  -- ┃     │ 39dbf9fc51c46377ad52a422fa9622f0e669d867c0cf294a9de89ea985ee610b     0        300000000000 lovelace + TxOutDatumNone
  -- ┃     │ 5c958373497564014fdff939ab3e30cbc4570b71b26a72136846db1c9700ddd7     0        299999999300 lovelace + TxOutDatumNone
  -- ┃     │ 6681e0758683cd2c3c637b66775e414f42131eff59a0bf5210f302fa0a082d13     0        300000000000 lovelace + TxOutDatumNone
  -- ┃     │ 7302912e58cd53edc505512795f691069711ffa01fb90018d0b7385a32769925     0        300000000000 lovelace + TxOutDatumNone
  -- ┃     │ 8b6a647492e5b063ac93014b09dc34941446f68e632e0cef79545e540a1dbbc2     0        299999999390 lovelace + TxOutDatumNone
  -- ┃     │ bf87e043a9113e3e33b74c307cec294d3abe44b267b0bb17a71bbc5463e8a106     0        2666666666666667 lovelace + TxOutDatumNone
  -- ┃     │ ccfabd1b3f293f3cf4ac8a7109c93aadd1540f196e341562827c204743a1c67a     0        2666666666666667 lovelace + TxOutDatumNone
  -- ┃     │ e2075175e3916afdb96922ca4ed519127e937f0856fa7e2bbd7a0fab17965e78     0        300000000000 lovelace + TxOutDatumNone

  -- Sometimes fails with:
  -- Command: /home/churlin/dev/cardano-node/dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-cli-8.19.0.0/x/cardano-cli/build/cardano-cli/cardano-cli conway transaction submit --testnet-magic 42 --tx-file /tmp/nix-shell.SbhJvi/propose-new-constitution-no-retries-test-a4bc13ec60c502bb/work/tx.body.signed
  -- ┃     │ Process exited with non-zero exit-code: 1
  -- ┃     │ ━━━━ command ━━━━
  -- ┃     │ cardano-cli conway transaction submit --testnet-magic 42 --tx-file /tmp/nix-shell.SbhJvi/propose-new-constitution-no-retries-test-a4bc13ec60c502bb/work/tx.body.signed
  -- ┃     │ ━━━━ stderr ━━━━
  -- ┃     │ Command failed: transaction submit  Error: Error while submitting tx: ShelleyTxValidationError ShelleyBasedEraConway (ApplyTxError [ConwayUtxowFailure (UtxoFailure (AlonzoInBabbageUtxoPredFailure (ValueNotConservedUTxO (MaryValue (Coin 0) (MultiAsset (fromList []))) (MaryValue (Coin 300000000000) (MultiAsset (fromList [])))))),ConwayUtxowFailure (UtxoFailure (AlonzoInBabbageUtxoPredFailure (BadInputsUTxO (fromList [TxIn (TxId {unTxId = SafeHash "dfb058a79a27794e5719507f6fb1bd8b6ddbd12d013b7a143f539eac671586f9"}) (TxIx 0)]))))])

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", txbodySignedFp
    ]

  H.threadDelay 3_000_000

  txidString <- mconcat . lines <$> H.execCli' execConfig
    [ "transaction", "txid"
    , "--tx-file", txbodySignedFp
    ]

  !propSubmittedResult
    <- runExceptT $ handleIOExceptT IOE
                  $ runExceptT $ foldBlocks
                      (File $ configurationFile testnetRuntime)
                      (File socketPath)
                      FullValidation
                      Nothing -- Initial accumulator state
                      (foldBlocksCheckProposalWasSubmitted (fromString txidString))

  newProposalEvents <- case propSubmittedResult of
                        Left (IOE e) ->
                          H.failMessage callStack
                            $ "foldBlocksCheckProposalWasSubmitted failed with: " <> show e
                        Right (Left e) ->
                          H.failMessage callStack
                            $ "foldBlocksCheckProposalWasSubmitted failed with: " <> Text.unpack (renderFoldBlocksError e)
                        Right (Right events) -> return events

  governanceActionIndex <- retrieveGovernanceActionIndex newProposalEvents

  let voteFp :: Int -> FilePath
      voteFp n = work </> gov </> "vote-" <> show n

  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified
  forM_ [1..3] $ \n -> do
    H.execCli' execConfig
      [ "conway", "governance", "vote", "create"
      , "--yes"
      , "--governance-action-tx-id", txidString
      , "--governance-action-index", show @Word32 governanceActionIndex
      , "--drep-verification-key-file", drepVkeyFp n
      , "--out-file", voteFp n
      ]

  -- Retrieve UTxOs
  H.note_ =<< H.execCli' execConfig
    [ "conway", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-4.json"
    ]

  txin4 <- do
    utxoJson <- H.leftFailM . H.readJsonFile $ work </> "utxo-4.json"
    UTxO utxo <- H.noteShowM $ decodeEraUTxO sbe utxoJson
    H.noteShow =<< H.headM (Map.keys utxo)
  -- Done obtaining UTxOs

  voteTxFp <- H.note $ work </> gov </> "vote.tx"
  govStateBefore <- H.note $ work </> gov </> "gov-state-before.txt"
  voteTxBodyFp <- H.note $ work </> gov </> "vote.txbody"
  govStateAfter <- H.note $ work </> gov </> "gov-state-after.txt"

  -- Submit votes
  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-in", Text.unpack $ renderTxIn txin4
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ wallets !! 1
    , "--vote-file", voteFp 1
    , "--vote-file", voteFp 2
    , "--vote-file", voteFp 3
    , "--witness-override", show @Int 4
    , "--out-file", voteTxBodyFp
    ]


  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", voteTxBodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 1
    , "--signing-key-file", drepSKeyFp 1
    , "--signing-key-file", drepSKeyFp 2
    , "--signing-key-file", drepSKeyFp 3
    , "--out-file", voteTxFp
    ]

  let strToLBS = TL.encodeUtf8 . TL.pack

  -- If we wanted the constitution alone, we would use "conway query constitution"
  jsonGovStateBeforeValue :: Aeson.Value <- fromJust . Aeson.decode . strToLBS <$> H.execCli' execConfig
    [ "conway", "query", "gov-state"
    , "--testnet-magic", show @Int testnetMagic
    ]
  -- H.note_ $ TL.unpack . TL.decodeUtf8 $ Aeson.encode jsonGovStateBeforeValue -- Too verbose
  enactStateBefore <- Aeson.assertHasKey "enactState" jsonGovStateBeforeValue
  constitutionBefore <- Aeson.assertHasKey "constitution" enactStateBefore
  H.note_ "Constitution before"
  H.note_ $ TL.unpack . TL.decodeUtf8 $ Aeson.encode constitutionBefore

  H.note_ =<< H.execCli' execConfig
    [ "conway", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", voteTxFp
    ]

  !eConstitutionAdopted
    <- runExceptT $ handleIOExceptT IOE
                  $ runExceptT $ foldBlocks
                      (File $ configurationFile testnetRuntime)
                      (File socketPath)
                      FullValidation
                      [] -- Initial accumulator state
                      (foldBlocksCheckConstitutionWasRatified constitutionHash)

  case eConstitutionAdopted of
    Left (IOE e) ->
      H.failMessage callStack
        $ "foldBlocksCheckConstitutionWasRatified failed with: " <> show e
    Right (Left e) ->
      H.failMessage callStack
        $ "foldBlocksCheckConstitutionWasRatified failed with: " <> Text.unpack (renderFoldBlocksError e)
    Right (Right _events) -> success

  H.threadDelay (2 * cardanoEpochLength) -- Wait 2 epochs, one should suffice, but let's be safe

  jsonGovStateAfterValue :: Aeson.Value <- fromJust . Aeson.decode . strToLBS <$> H.execCli' execConfig
    [ "conway", "query", "gov-state"
    , "--testnet-magic", show @Int testnetMagic
    ]
  -- H.note_ $ TL.unpack . TL.decodeUtf8 $ Aeson.encode jsonGovStateAfterValue -- Too verbose
  enactState <- Aeson.assertHasKey "enactState" jsonGovStateAfterValue
  constitutionAfter <- Aeson.assertHasKey "constitution" enactState
  H.note_ "Constitution after"
  H.note_ $ TL.unpack . TL.decodeUtf8 $ Aeson.encode constitutionAfter

  case expectation of
    ConstitutionUnchanged -> do
       constitutionBefore H.=== constitutionAfter
    ConstitutionChanged -> do
       constitutionBefore H./== constitutionAfter

foldBlocksCheckProposalWasSubmitted
  :: TxId -- TxId of submitted tx
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> Maybe LedgerEvent -- ^ Accumulator at block i - 1
  -> IO (Maybe LedgerEvent, FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksCheckProposalWasSubmitted txid _ _ allEvents _ _ = do
  let newGovProposal = filter (filterNewGovProposals txid) allEvents
  if null newGovProposal
  then return (Nothing, ContinueFold)
  else return (Just $ head newGovProposal , StopFold)


retrieveGovernanceActionIndex
  :: (HasCallStack, MonadTest m)
  => Maybe LedgerEvent -> m Word32
retrieveGovernanceActionIndex mEvent = do
  case mEvent of
    Nothing -> H.failMessage callStack "retrieveGovernanceActionIndex: No new governance proposals found"
    Just (NewGovernanceProposals _ (AnyProposals props)) ->
    -- In this test there will only be one
        let govActionStates = [i
                              | Ledger.GovActionIx i <- map Ledger.gaidGovActionIx . Map.keys $ Ledger.proposalsGovActionStates props
                              ]
        in return $ head  govActionStates
    Just unexpectedEvent ->
      H.failMessage callStack
        $ mconcat ["retrieveGovernanceActionIndex: Expected NewGovernanceProposals, got: "
                  , show unexpectedEvent
                  ]


filterNewGovProposals :: TxId -> LedgerEvent -> Bool
filterNewGovProposals txid (NewGovernanceProposals eventTxId (AnyProposals props)) =
  let _govActionStates = Ledger.proposalsGovActionStates props
  in fromShelleyTxId eventTxId == txid
filterNewGovProposals _ _ = False

foldBlocksCheckConstitutionWasRatified
  :: String -- submitted constitution hash
  -> Env
  -> LedgerState
  -> [LedgerEvent]
  -> BlockInMode -- Block i
  -> [LedgerEvent] -- ^ Accumulator at block i - 1
  -> IO ([LedgerEvent], FoldStatus) -- ^ Accumulator at block i and fold status
foldBlocksCheckConstitutionWasRatified submittedConstitutionHash _ _ allEvents _ _ =
  if any (filterRatificationState submittedConstitutionHash) allEvents
  then return (allEvents , StopFold)
  else return ([], ContinueFold)

filterRatificationState
  :: String -- ^ Submitted constitution anchor hash
  -> LedgerEvent
  -> Bool
filterRatificationState c (EpochBoundaryRatificationState (AnyRatificationState rState)) =
  let constitutionAnchorHash = Ledger.anchorDataHash $ Ledger.constitutionAnchor (rState ^. Ledger.rsEnactStateL . Ledger.ensConstitutionL)
  in Text.pack c == renderSafeHashAsHex constitutionAnchorHash
filterRatificationState _ _ = False