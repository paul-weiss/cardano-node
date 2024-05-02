{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
import           Cardano.TxGenerator.Types (NumberOfTxs, TPSRate, TxGenError (..))

import           Prelude (String)

import           Control.Arrow ((+++))
import qualified Control.Concurrent.STM as STM
import           Control.Exception (throw)
import qualified Data.ByteString.Char8 as BS
import           Data.Functor.Adjunction (uncozipL)
import qualified Data.List as List (intercalate, unlines)
import qualified Data.List.NonEmpty as NE
import           Data.Text (pack)
import qualified Data.Time.Clock as Clock
import           GHC.IO.Exception (IOErrorType (..), IOException (..))
import           Network.DNS (DNSError (DecodeError), Domain, defaultResolvConf, lookupRDNS,
                   makeResolvSeed, withResolver)
import           Network.Socket (AddrInfo (..), AddrInfoFlag (..), Family (..), SockAddr (..),
                   SocketType (Stream), addrFamily, addrFlags, addrSocketType, defaultHints,
                   getAddrInfo, hostAddress6ToTuple, hostAddressToTuple)
import           Numeric (showHex)


type AsyncBenchmarkControl = (Async (), [Async ()], IO SubmissionSummary, IO ())

waitBenchmark :: Trace IO (TraceBenchTxSubmit TxId) -> AsyncBenchmarkControl -> ExceptT TxGenError IO ()
waitBenchmark traceSubmit (feeder, workers, mkSummary, _) = liftIO $ do
  mapM_ waitCatch (feeder : workers)
  traceWith traceSubmit . TraceBenchTxSubSummary =<< mkSummary

lookupNodeAddress ::
  NodeAddress' NodeHostIPv4Address -> IO AddrInfo
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

addrInfoToName :: AddrInfo -> Either FilePath Domain
addrInfoToName AddrInfo {..} =
  case addrAddress of
    SockAddrInet  _port (hostAddressToTuple -> _addr@(b1,b2,b3,b4)) ->
      Right $ mkIPname "." show [b1,b2,b3,b4]
    SockAddrInet6 _port _flowInfo addr6 _scopeID ->
      let (b1,b2,b3,b4,b5,b6,b7,b8) = hostAddress6ToTuple addr6
       in Right $ mkIPname ":" showHex' [b1,b2,b3,b4,b5,b6,b7,b8]
    SockAddrUnix path -> Left path
  where
    mkIPname :: ByteString -> (t -> String) -> [t] -> ByteString
    mkIPname separator render =
      BS.intercalate separator . map (BS.pack . render)
    showHex' :: (Integral t, Show t) => t -> String
    showHex' 0 = ""
    showHex' n = showHex n ""

handleTxSubmissionClientError ::
     Trace IO (TraceBenchTxSubmit TxId)
  -> Network.Socket.AddrInfo
  -> ReportRef
  -> SubmissionErrorPolicy
  -> SomeException
  -> IO ()
handleTxSubmissionClientError
  traceSubmit
  remoteAddr
  reportRef
  errorPolicy
  (SomeException err) = do
    resolveSeed <- makeResolvSeed defaultResolvConf
    _errorOrName :: Either DNSError [Domain]
                <- withResolver resolveSeed \resolver ->
                      liftM join . uncozipL .
                          (pure . mkUnixError +++ lookupRDNS resolver) $
                                  addrInfoToName remoteAddr
    submitThreadReport reportRef (Left errDesc)
    case errorPolicy of
      FailOnError -> throwIO err
      LogErrors   -> traceWith traceSubmit $
        TraceBenchTxSubError (pack errDesc)
   where
    mkUnixError :: String -> DNSError
    mkUnixError path =
      DecodeError $ "Unix domain socket passed to reverse DNS: " ++ path
    errDesc = mconcat
      [ "Exception while talking to peer "
      , " (", show (addrAddress remoteAddr), "): "
      , show err]
    _mkDNSErr :: DNSError -> IO IOException
    _mkDNSErr dnsError = do
         let ioe_description = List.unlines $
               [ List.intercalate " " $
                   [ "Encountered Unix domain socket attempting to resolve"
                   , "remote address"
                   , show remoteAddr
                   , "to hostname via reverse DNS:"
                   , show dnsError ]
               , "in an attempt to handle the following exception: "
               , displayException err ]
         throw IOError { ioe_handle   = Nothing
                       , ioe_type     = InvalidArgument
                       , ioe_errno    = Nothing
                       , ioe_filename = Just "GeneratorTx.hs"
                       , ioe_location = "handleTxSubmissionClientError"
                       , .. }
      -- Right               [] -> error "rightSide empty"
      -- Right _rightSide@(_:_) -> error "rightSide non-empty"

walletBenchmark :: forall era. IsShelleyBasedEra era
  => Trace IO (TraceBenchTxSubmit TxId)
  -> Trace IO NodeToNodeSubmissionTrace
  -> ConnectClient
  -> String
  -> NonEmpty NodeIPv4Address
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

  remoteAddresses <- forM targets lookupNodeAddress
  let numTargets :: Natural = fromIntegral $ NE.length targets

  traceDebug $ "******* Tx generator, launching Tx peers:  " ++ show (NE.length remoteAddresses) ++ " of them"

  startTime <- Clock.getCurrentTime
  tpsThrottle <- newTpsThrottle 32 count tpsRate

  reportRefs <- STM.atomically $ replicateM (fromIntegral numTargets) STM.newEmptyTMVar

  txStreamRef <- newMVar $ StreamActive txSource
  allAsyncs <- forM (zip reportRefs $ NE.toList remoteAddresses) $
    \(reportRef, remoteAddr) -> do
      let errorHandler = handleTxSubmissionClientError traceSubmit remoteAddr reportRef errorPolicy
          client = txSubmissionClient
                     traceN2N
                     traceSubmit
                     (txStreamSource txStreamRef tpsThrottle)
                     (submitSubmissionThreadStats reportRef)
      async $ handle errorHandler (connectClient remoteAddr client)

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
