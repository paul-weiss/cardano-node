{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Testnet.Test.Gov.UpdateCommittee
  ( hprop_gov_update_committee
  ) where

import           Cardano.Api as Api
import           Cardano.Api.Error
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Conway.Genesis as L
import qualified Cardano.Ledger.Conway.Governance as L
import qualified Cardano.Ledger.Credential as L
import           Cardano.Testnet

import           Prelude

import           Control.Monad
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import           Data.String
import qualified Data.Text as Text
import           GHC.Stack
import           System.FilePath ((</>))

import           Testnet.Components.Configuration
import           Testnet.Components.DReps as DRep
import           Testnet.Components.Query
import           Testnet.Components.SPO as SPO
import           Testnet.Components.TestWatchdog
import           Testnet.Defaults
import qualified Testnet.Process.Cli as P
import qualified Testnet.Process.Run as H
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import           Hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as IO

-- | Execute me with:
-- @DISABLE_RETRIES=1 cabal test cardano-testnet-test --test-options '-p "/UpdateCommittee/"'@
-- Generate a testnet with a committee defined in the Conway genesis. Add and remove members from the committee
-- in a single governance proposal.
hprop_gov_update_committee :: Property
hprop_gov_update_committee = H.integrationWorkspace "update-committee" $ \tempAbsBasePath' -> runWithDefaultWatchdog_ $ do
  -- Start a local test net
  conf@Conf { tempAbsPath } <- mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
      tempBaseAbsPath = makeTmpBaseAbsPath tempAbsPath

  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"


  let ceo = ConwayEraOnwardsConway
      sbe = conwayEraOnwardsToShelleyBasedEra ceo
      era = toCardanoEra sbe
      cEra = AnyCardanoEra era
      fastTestnetOptions = cardanoDefaultTestnetOptions
        { cardanoEpochLength = 100
        , cardanoNodeEra = cEra
        }
  execConfigOffline <- H.mkExecConfigOffline tempBaseAbsPath

  -- Step 1. Define generate and define a committee in the genesis file

  -- Create committee cold keys
  H.createDirectoryIfMissing_ $ tempAbsPath' </> work </> "committee-keys"
  H.forConcurrently_ [1..3] $ \n -> do
    H.execCli' execConfigOffline
      [ anyEraToString cEra, "governance", "committee"
      , "key-gen-cold"
      , "--cold-verification-key-file", work </> defaultCommitteeVkeyFp n
      , "--cold-signing-key-file", work </> defaultCommitteeSkeyFp n
      ]

  committeeVkey1Fp <- H.noteShow $ work </> defaultCommitteeVkeyFp 1
  _committeeSkey1Fp <- H.noteShow $ work </> defaultCommitteeSkeyFp 1
  committeeVkey2Fp <- H.noteShow $ work </> defaultCommitteeVkeyFp 2
  _committeeSkey2Fp <- H.noteShow $ work </> defaultCommitteeSkeyFp 2

  -- This is the member we will first elect i.e not put into the
  -- conway genesis.
  committeeVkey3Fp <- H.noteShow $ work </> defaultCommitteeVkeyFp 3
  _committeeSkey3Fp <- H.noteShow $ work </> defaultCommitteeSkeyFp 3

  -- Read committee cold keys from disk to put into conway genesis

  comKeyHash1Str <- filter (/= '\n') <$> H.execCli' execConfigOffline
      [ anyEraToString cEra, "governance", "committee"
      , "key-hash"
      , "--verification-key-file", committeeVkey1Fp
      ]

  _comKeyHash2Str <- H.execCli' execConfigOffline
      [ anyEraToString cEra, "governance", "committee"
      , "key-hash"
      , "--verification-key-file", committeeVkey2Fp
      ]

  CommitteeColdKeyHash comKeyHash1 <-
    H.evalEither
      $ deserialiseFromRawBytesHex (AsHash AsCommitteeColdKey)
      $ BSC.pack comKeyHash1Str

  let comKeyCred1 = L.KeyHashObj comKeyHash1
      committeeThreshold = unsafeBoundedRational 0.5
      committee = L.Committee (Map.fromList [(comKeyCred1, EpochNo 100)]) committeeThreshold

  alonzoGenesis <- evalEither $ first prettyError defaultAlonzoGenesis
  (startTime, shelleyGenesis') <- getDefaultShelleyGenesis fastTestnetOptions
  let conwayGenesisWithCommittee =
        defaultConwayGenesis { L.cgCommittee = committee }
  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets=wallet0:_wallet1:_
    , configurationFile
    } <- cardanoTestnet
           fastTestnetOptions
           conf startTime shelleyGenesis'
           alonzoGenesis conwayGenesisWithCommittee

  poolNode1 <- H.headM poolNodes
  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1
  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1 testnetMagic

  let socketName' = IO.sprocketName poolSprocket1
      socketBase = IO.sprocketBase poolSprocket1 -- /tmp
      socketPath = socketBase </> socketName'

  _epochStateView <- getEpochStateView (File configurationFile) (File socketPath)

  H.note_ $ "Sprocket: " <> show poolSprocket1
  H.note_ $ "Abs path: " <> tempAbsBasePath'
  H.note_ $ "Socketpath: " <> socketPath

  -- Step 1. We will first propose to add an additional member to the
  -- constitutional committeee. DRep and SPO voting thresholds must be met.

  -- Create proposal to add a new member to the committee

  proposalAnchorFile <- H.note $ work </> "sample-proposal-anchor"
  H.writeFile proposalAnchorFile "dummy anchor data"

  proposalAnchorDataHash <- H.execCli' execConfig
    [ eraToString era, "governance"
    , "hash", "anchor-data", "--file-text", proposalAnchorFile
    ]


  proposalFile <- H.note $ work </> "sample-proposal-anchor"
  stakeVkeyFp <- H.note $ work </> "stake.vkey"
  stakeSKeyFp <- H.note $ work </> "stake.skey"

  _ <- P.cliStakeAddressKeyGen tempAbsPath'
         $ P.KeyNames { P.verificationKeyFile = stakeVkeyFp
                      , P.signingKeyFile = stakeSKeyFp
                      }
  void $ H.execCli' execConfig $
    [ eraToString era, "governance", "action", "update-committee"
    , "--testnet"
    , "--governance-action-deposit", show @Integer 1_000_000 --- TODO: Get from protocol parameters
    , "--deposit-return-stake-verification-key-file", stakeVkeyFp
    , "--anchor-url", "https://tinyurl.com/3wrwb2as"
    , "--anchor-data-hash", proposalAnchorDataHash
    , "--add-cc-cold-verification-key-file", committeeVkey3Fp
    , "--epoch", "1000"
    , "--threshold", "0.5"
    , "--out-file", proposalFile
    ]

  txbodyFp <- H.note $ work </> "tx.body"
  epochStateView <- getEpochStateView (File configurationFile) (File socketPath)

  txin1 <- findLargestUtxoForPaymentKey epochStateView sbe wallet0

  void $ H.execCli' execConfig
    [ eraToString era, "transaction", "build"
    , "--change-address", Text.unpack $ paymentKeyInfoAddr wallet0
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr wallet0) <> "+" <> show @Int 5_000_000
    , "--proposal-file", proposalFile
    , "--out-file", txbodyFp
    ]

  signedProposalTx <- signTx execConfig cEra work "signed-proposal"
                           (File txbodyFp) [paymentKeyInfoPair wallet0]

  submitTx execConfig cEra signedProposalTx

  -- Create and submit votes on committee update proposal
  -- Proposal was successfully submitted, now we vote on the proposal and confirm it was ratified

  governanceActionTxId <- retrieveTransactionId execConfig signedProposalTx

  !propSubmittedResult <- findCondition (maybeExtractGovernanceActionIndex sbe (fromString governanceActionTxId))
                                        configurationFile
                                        socketPath
                                        (EpochNo 10)

  governanceActionIndex <- case propSubmittedResult of
                             Left e ->
                               H.failMessage callStack
                                 $ "findCondition failed with: " <> displayError e
                             Right Nothing ->
                               H.failMessage callStack "Couldn't find proposal."
                             Right (Just a) -> return a

  let spoVotes :: [(String, Int)]
      spoVotes =  [("yes", 1), ("yes", 2), ("yes", 3)]
      drepVotes :: [(String, Int)]
      drepVotes = [("yes", 1), ("yes", 2), ("yes", 3)]


  annotateShow spoVotes
  annotateShow drepVotes

  voteFiles <- error "TODO"
                   --SPO.generateVoteFiles execConfig work "vote-files"
                   --              governanceActionTxId governanceActionIndex
                   --              $ error "TODO" --[(defaultSPOKeys idx, vote) | (vote, idx) <- spoVotes]

  -- Submit votes
  voteTxBodyFp <- createVotingTxBody execConfig epochStateView sbe work "vote-tx-body"
                                     voteFiles wallet0

  voteTxFp <- error "TODO"
              -- signTx execConfig cEra work "signed-vote-tx" voteTxBodyFp
              --       $ error "TODO"
                     --(paymentKeyInfoPair wallet0:[defaultSPOKeyPair n | (_, n) <- spoVotes])
                     -- TODO: voting SPOs and DReps need to sign this

  submitTx execConfig cEra voteTxFp
  -- Confirm the proposal has been ratified

  return ()

