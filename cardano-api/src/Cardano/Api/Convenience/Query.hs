{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Convenience query functions
--
module Cardano.Api.Convenience.Query (
    QueryConvenienceError(..),
    determineEra,
    determineEra_,
    -- * Simplest query related
    executeQueryCardanoMode,
    queryStateForBalancedTx,

    queryUtxo_,
    queryProtocolParams_,
    queryEraHistory_,
    queryStakePools_,
    querySystemStart_,

    renderQueryConvenienceError,
  ) where

import           Control.Monad.Oops (CouldBe, Variant, runOopsInEither)
import qualified Control.Monad.Oops as OO
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT, left, newExceptT, onLeft,
                   onNothing)
import           Data.Bifunctor (first)
import           Data.Function ((&))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text

import           Cardano.Api.Certificate
import           Cardano.Api.Convenience.Constraints
import           Cardano.Api.Convenience.Error
import           Cardano.Api.Environment
import           Cardano.Api.Eras
import           Cardano.Api.IPC
import           Cardano.Api.IPC.AnyQuery
import           Cardano.Api.IPC.Monad (LocalStateQueryExpr, queryExpr_)
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query
import           Cardano.Api.Query.ShelleyBased
import           Cardano.Api.TxBody
import           Cardano.Api.Utils

import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..))

renderQueryConvenienceError :: QueryConvenienceError -> Text
renderQueryConvenienceError (AcqFailure e) =
  "Acquiring failure: " <> textShow e
renderQueryConvenienceError (SockErr e) =
  renderEnvSocketError e
renderQueryConvenienceError (QueryEraMismatch (EraMismatch ledgerEraName' otherEraName')) =
  "The era of the node and the tx do not match. " <>
  "The node is running in the " <> ledgerEraName' <>
  " era, but the transaction is for the " <> otherEraName' <> " era."
renderQueryConvenienceError ByronEraNotSupported =
  "Byron era not supported"
renderQueryConvenienceError (EraConsensusModeMismatch cMode anyCEra) =
  "Consensus mode and era mismatch. Consensus mode: " <> textShow cMode <>
  " Era: " <> textShow anyCEra
renderQueryConvenienceError (QueryConvenienceError e) = Text.pack $ show e

queryUtxo_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` EraMismatch
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> [TxIn]
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (AnyQuery mode) r IO) (UTxO era)
queryUtxo_ qeInMode qSbe allTxIns = do
  let query = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra qeInMode $ QueryInShelleyBasedEra qSbe $
        QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))

  queryExpr_ query & OO.onLeft @EraMismatch OO.throw

queryProtocolParams_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` EraMismatch
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (AnyQuery mode) r IO) ProtocolParameters
queryProtocolParams_ qeInMode qSbe = do
  let query = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra qeInMode $ QueryInShelleyBasedEra qSbe QueryProtocolParameters

  queryExpr_ query & OO.onLeft @EraMismatch OO.throw

queryEraHistory_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => ExceptT (Variant e) (LocalStateQueryExpr block point (AnyQuery CardanoMode) r IO) (EraHistory CardanoMode)
queryEraHistory_ = do
  let query = AnyQueryAnyEra $ QueryEraHistory CardanoModeIsMultiEra

  queryExpr_ query

queryStakePools_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => e `CouldBe` QueryConvenienceError
  => EraInMode era mode
  -> ShelleyBasedEra era
  -> ExceptT (Variant e) (LocalStateQueryExpr block point (AnyQuery mode) r IO) (Set PoolId)
queryStakePools_ qeInMode qSbe = do
  let query = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra qeInMode $ QueryInShelleyBasedEra qSbe $ QueryStakePools

  queryExpr_ query & OO.onLeft @EraMismatch (OO.throw . QueryEraMismatch)

querySystemStart_ :: ()
  => e `CouldBe` UnsupportedNtcVersionError
  => ExceptT (Variant e) (LocalStateQueryExpr block point (AnyQuery mode) r IO) SystemStart
querySystemStart_ = queryExpr_ $ AnyQueryAnyEra QuerySystemStart

-- | A convenience function to query the relevant information, from
-- the local node, for Cardano.Api.Convenience.Construction.constructBalancedTx
queryStateForBalancedTx
  :: NetworkId
  -> [TxIn]
  -> IO (Either QueryConvenienceError (AnyUTxO, ProtocolParameters, EraHistory CardanoMode, SystemStart, Set PoolId))
queryStateForBalancedTx networkId allTxIns = runExceptT $ do
  SocketPath sockPath <- newExceptT $ first SockErr <$> readEnvSocketPath
  let cModeParams = CardanoModeParams $ EpochSlots 21600
      localNodeConnInfo = LocalNodeConnectInfo
                            cModeParams
                            networkId
                            sockPath
  firstExceptT QueryConvenienceError $ newExceptT
    $ executeLocalStateQueryExprAnyQuery localNodeConnInfo Nothing $ do
        AnyCardanoEra era <- determineEraExprAnyQuery cModeParams
        eInMode <- determineEraInModeAnyQuery era cModeParams
        case cardanoEraStyle era of
          LegacyByronEra -> left AllQueryEraExpectedSbe
          ShelleyBasedEra sbe -> do
            let utxoQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode $ QueryInShelleyBasedEra sbe
                              $ QueryUTxO (QueryUTxOByTxIn (Set.fromList allTxIns))
                pparamsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                 $ QueryInShelleyBasedEra sbe QueryProtocolParameters
                eraHistoryQuery = AnyQueryAnyEra $ QueryEraHistory CardanoModeIsMultiEra
                systemStartQuery = AnyQueryAnyEra QuerySystemStart
                stakePoolsQuery = AnyQueryShelleyBasedEra $ QueryShelleyBasedEra eInMode
                                    $ QueryInShelleyBasedEra sbe QueryStakePools
            utxo <- AnyUTxO (shelleyBasedToCardanoEra sbe) <$> queryExprAnyQueryE utxoQuery
            pparams <- queryExprAnyQueryE pparamsQuery
            eraHistory <- queryExprAnyQuery eraHistoryQuery
            systemStart <- queryExprAnyQuery systemStartQuery
            stakePools <- queryExprAnyQueryE stakePoolsQuery
            return (utxo, pparams, eraHistory, systemStart, stakePools)

-- | Query the node to determine which era it is in.
determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> IO (Either AcquiringFailure AnyCardanoEra)
determineEra cModeParams localNodeConnInfo =
  runOopsInEither $ determineEra_ cModeParams localNodeConnInfo

-- | Query the node to determine which era it is in.
determineEra_
  :: forall e mode. ()
  => e `CouldBe` AcquiringFailure
  => ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> ExceptT (Variant e) IO AnyCardanoEra
determineEra_ cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> pure $ AnyCardanoEra ByronEra
    ShelleyMode -> pure $ AnyCardanoEra ShelleyEra
    CardanoMode ->
      queryNodeLocalState_ localNodeConnInfo Nothing
        $ QueryCurrentEra CardanoModeIsMultiEra

-- | Execute a query against the local node. The local
-- node must be in CardanoMode.
executeQueryCardanoMode
  :: CardanoEra era
  -> NetworkId
  -> QueryInMode CardanoMode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryCardanoMode era nid q = runExceptT $ do
  SocketPath sockPath <- firstExceptT SockErr . ExceptT $ readEnvSocketPath

  let localNodeConnInfo =
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
          , localNodeNetworkId = nid
          , localNodeSocketPath = sockPath
          }

  ExceptT $ executeQueryAnyMode era localNodeConnInfo q

-- | Execute a query against the local node in any mode.
executeQueryAnyMode
  :: forall result era mode. CardanoEra era
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> IO (Either QueryConvenienceError result)
executeQueryAnyMode era localNodeConnInfo q = runExceptT $ do
  let cMode = consensusModeOnly $ localConsensusModeParams localNodeConnInfo

  eraInMode <- pure (toEraInMode era cMode)
    & onNothing (left $ EraConsensusModeMismatch
        (AnyConsensusMode CardanoMode)
        (getIsCardanoEraConstraint era $ AnyCardanoEra era))

  case eraInMode of
    ByronEraInByronMode -> left ByronEraNotSupported
    _ ->
      lift (queryNodeLocalState localNodeConnInfo Nothing q)
        & onLeft (left . AcqFailure)
        & onLeft (left . QueryEraMismatch)
