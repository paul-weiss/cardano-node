{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Benchmarking.Script
  ( Script
  , runScript
  , parseScriptFileAeson
  )
where

import           Cardano.Benchmarking.LogTypes
import           Cardano.Benchmarking.Script.Action
import           Cardano.Benchmarking.Script.Aeson (parseScriptFileAeson)
import           Cardano.Benchmarking.Script.Core (setProtocolParameters)
import qualified Cardano.Benchmarking.Script.Env as Env (ActionM, Env (Env, envThreads),
                   Error (TxGenError), getEnvThreads, runActionMEnv, traceError)
import           Cardano.Benchmarking.Script.Types
import qualified Cardano.TxGenerator.Types as Types (TxGenError (..))
import           Ouroboros.Network.NodeToClient (IOManager)

import           Prelude

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM.TVar as STM (readTVar)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM as STM (atomically)
import           Control.Monad.Trans.Except as Except (throwE)
import qualified Data.List as List (unwords)
import qualified Data.Map as Map (lookup)
import           System.Mem (performGC)

type Script = [Action]

runScript :: Env.Env -> Script -> IOManager -> IO (Either Env.Error (), AsyncBenchmarkControl)
runScript env script iom = do
  result <- go
  performGC
  threadDelay $ 150 * 1_000
  return result
  where
    go :: IO (Either Env.Error (), AsyncBenchmarkControl)
    go = Env.runActionMEnv env execScript iom >>= \case
      (Right abc, env', ()) -> do
        cleanup env' shutDownLogging
        pure (Right (), abc)
      (Left err,  env'@Env.Env { .. }, ()) -> do
        cleanup env' (Env.traceError (show err) >> shutDownLogging)
        case "tx-submit-benchmark" `Map.lookup` envThreads of
          Just abcTVar -> do
            abcMaybe <- STM.atomically $ STM.readTVar abcTVar
            case abcMaybe of
              Just abc -> pure (Left err, abc)
              Nothing  -> error $ List.unwords
                                    [ "Cardano.Benchmarking.Script.runScript:"
                                    , "AsyncBenchmarkControl uninitialized" ]
          Nothing  -> error $ List.unwords
                                [ "Cardano.Benchmarking.Script.runScript:"
                                , "AsyncBenchmarkControl absent from map" ]
      where
        cleanup :: Env.Env -> Env.ActionM () -> IO ()
        cleanup env' acts = void $ Env.runActionMEnv env' acts iom
        execScript :: Env.ActionM AsyncBenchmarkControl
        execScript = do
          setProtocolParameters QueryLocalNode
          forM_ script action
          abcMaybe <- Env.getEnvThreads "tx-submit-benchmark"
          case abcMaybe of
            Nothing  -> throwE $ Env.TxGenError $ Types.TxGenError $
              List.unwords
                  [ "Cardano.Benchmarking.Script.runScript:"
                  , "AsyncBenchmarkControl absent from map in execScript" ]
            Just abc -> pure abc

shutDownLogging :: Env.ActionM ()
shutDownLogging = do
  Env.traceError "QRT Last Message. LoggingLayer going to shutdown. 73 . . . ."
  liftIO $ threadDelay $ 350 * 1_000
