{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Redundant <$>" -}

module Cardano.Benchmarking.GeneratorTx
  ( AsyncBenchmarkControl
  , walletBenchmark
  -- , readSigningKey
  , waitBenchmark
  ) where

import           Cardano.Api hiding (txFee)

import           Cardano.Benchmarking.GeneratorTx.NodeToNode
import           Cardano.Benchmarking.GeneratorTx.Submission
import           Cardano.Benchmarking.GeneratorTx.SubmissionClient
import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.TpsThrottle
import           Cardano.Benchmarking.Types
import           Cardano.Benchmarking.Wallet (TxStream)
import           Cardano.Logging
import           Cardano.Node.Configuration.NodeAddress
import           Cardano.Prelude
import           Cardano.TxGenerator.Setup.NixService
import           Cardano.TxGenerator.Types (NumberOfTxs, TPSRate, TxGenError (..))

import           Prelude (String)

import qualified Control.Concurrent.STM as STM
import qualified Data.List.NonEmpty as NE
import           Data.Text (pack)
import qualified Data.Time.Clock as Clock
import           Data.Tuple.Extra (secondM)
import           GHC.Conc (labelThread)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SocketType (Stream),
                   addrFamily, addrFlags, addrSocketType, defaultHints, getAddrInfo)


type AsyncBenchmarkControl = (Async (), [Async ()], IO SubmissionSummary, IO ())

waitBenchmark :: Trace IO (TraceBenchTxSubmit TxId) -> AsyncBenchmarkControl -> ExceptT TxGenError IO ()
waitBenchmark traceSubmit (feeder, workers, mkSummary, _) = liftIO $ do
  mapM_ waitCatch (feeder : workers)
  traceWith traceSubmit . TraceBenchTxSubSummary =<< mkSummary

lookupNodeAddress :: NodeIPv4Address -> IO AddrInfo
lookupNodeAddress node = do
  (remoteAddr:_) <- getAddrInfo (Just hints) (Just targetNodeHost) (Just targetNodePort)
  return remoteAddr
 where
  targetNodeHost = show . unNodeHostIPv4Address $ naHostAddress node
  targetNodePort = show $ naPort node
  hints :: AddrInfo
  hints = defaultHints
    { addrFlags      = [AI_PASSIVE]
    , addrFamily     = AF_INET
    , addrSocketType = Stream
    , addrCanonName  = Nothing
    }

handleTxSubmissionClientError ::
     Trace IO (TraceBenchTxSubmit TxId)
  -> (String, Network.Socket.AddrInfo)
  -> ReportRef
  -> SubmissionErrorPolicy
  -> SomeException
  -> IO ()
handleTxSubmissionClientError
  traceSubmit
  (remoteName, remoteAddr)
  reportRef
  errorPolicy
  (SomeException err) = do
    submitThreadReport reportRef (Left errDesc)
    case errorPolicy of
      FailOnError -> throwIO err
      LogErrors   -> traceWith traceSubmit $
        TraceBenchTxSubError (pack errDesc)
   where
    errDesc = mconcat
      [ "Exception while talking to peer "
      , remoteName
      , " (", show (addrAddress remoteAddr), "): "
      , show err]

walletBenchmark :: forall era. IsShelleyBasedEra era
  => Trace IO (TraceBenchTxSubmit TxId)
  -> Trace IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> String
  -> NonEmpty NodeDescription
  -> TPSRate
  -> SubmissionErrorPolicy
  -> AsType era
-- this is used in newTpsThrottle to limit the tx-count !
-- This should not be needed, the stream should do it itself (but it does not!)
  -> NumberOfTxs
-- This is TxStream is used in a wrong way !
-- It is used multithreaded here ! And every thread gets its own copy of the stream !!
-- This is a BUG it only works by coincidence !
-- Todo: Use the stream behind an MVar !
  -> TxStream IO era
  -> ExceptT TxGenError IO AsyncBenchmarkControl
walletBenchmark
  traceSubmit
  traceN2N
  connectClient
  threadName
  targets
  tpsRate
  errorPolicy
  _era
  count
  txSource
  = liftIO $ do
  traceDebug "******* Tx generator, phase 2: pay to recipients *******"

  let numTargets :: Natural = fromIntegral $ NE.length targets
      lookupTarget :: NodeDescription -> IO (String, AddrInfo)
      lookupTarget NodeDescription {..} = secondM lookupNodeAddress (ndName, ndAddr)
  remoteAddresses <- forM targets lookupTarget

  traceDebug $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"

  startTime <- Clock.getCurrentTime
  tpsThrottle <- newTpsThrottle 32 count tpsRate

  reportRefs <- STM.atomically $ replicateM (fromIntegral numTargets) STM.newEmptyTMVar

  txStreamRef <- newMVar $ StreamActive txSource
  allAsyncs <- forM (zip reportRefs $ NE.toList remoteAddresses) $
    \(reportRef, remoteInfo@(remoteName, remoteAddrInfo)) -> do
      let errorHandler = handleTxSubmissionClientError traceSubmit remoteInfo reportRef errorPolicy
          client = txSubmissionClient
                     traceN2N
                     traceSubmit
                     (txStreamSource txStreamRef tpsThrottle)
                     (submitSubmissionThreadStats reportRef)
          remoteAddrString = show $ addrAddress remoteAddrInfo
      asyncThread <- async $ handle errorHandler (connectClient remoteAddrInfo client)
      let tid = asyncThreadId asyncThread
      labelThread tid $ "txSubmissionClient " ++ show tid ++
                            " servicing " ++ remoteName ++ " (" ++ remoteAddrString ++ ")"
      pure asyncThread

  tpsThrottleThread <- async $ do
    startSending tpsThrottle
    traceWith traceSubmit $ TraceBenchTxSubDebug "tpsLimitedFeeder : transmitting done"
    atomically $ sendStop tpsThrottle
    traceWith traceSubmit $ TraceBenchTxSubDebug "tpsLimitedFeeder : shutdown done"

  let tpsFeederShutdown = do
        cancel tpsThrottleThread
        liftIO $ atomically $ sendStop tpsThrottle

  return (tpsThrottleThread, allAsyncs, mkSubmissionSummary threadName startTime reportRefs, tpsFeederShutdown)
 where
  traceDebug :: String -> IO ()
  traceDebug =   traceWith traceSubmit . TraceBenchTxSubDebug
