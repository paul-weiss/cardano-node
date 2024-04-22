{-# LANGUAGE FlexibleInstances #-}

module Main
  ( main
  ) where

import qualified Cardano.Crypto.Init as Crypto
import qualified Cardano.Testnet.Test.Cli.Babbage.LeadershipSchedule
import qualified Cardano.Testnet.Test.Cli.Babbage.StakeSnapshot
import qualified Cardano.Testnet.Test.Cli.Babbage.Transaction
import qualified Cardano.Testnet.Test.Cli.Conway.DRepRetirement as DRepRetirement
import qualified Cardano.Testnet.Test.Cli.Conway.Plutus
import qualified Cardano.Testnet.Test.Cli.KesPeriodInfo
import qualified Cardano.Testnet.Test.Cli.Queries
import qualified Cardano.Testnet.Test.Cli.QuerySlotNumber
import qualified Cardano.Testnet.Test.Cli.TxBuildEstimate
import qualified Cardano.Testnet.Test.FoldBlocks
import qualified Cardano.Testnet.Test.LedgerEvents.Gov.DRepDeposits
import qualified Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitution
import qualified Cardano.Testnet.Test.LedgerEvents.Gov.ProposeNewConstitutionSPO as LedgerEvents
import qualified Cardano.Testnet.Test.LedgerEvents.SanityCheck as LedgerEvents
import qualified Cardano.Testnet.Test.LedgerEvents.TreasuryGrowth as LedgerEvents
import qualified Cardano.Testnet.Test.Node.Shutdown
import qualified Cardano.Testnet.Test.SubmitApi.Babbage.Transaction

import           Prelude

import qualified System.Environment as E
import qualified System.Exit as IO
import qualified System.IO as IO
import           System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)

import qualified Testnet.Property.Run as H

import qualified Test.Tasty as T
import           Test.Tasty (TestTree)
import qualified Test.Tasty.Ingredients as T
import qualified Test.Tasty.Options as T
import qualified Test.Tasty.Runners as T

tests :: IO TestTree
tests = do
  pure $ T.testGroup "test/Spec.hs"
    [ T.testGroup "Transaction"
        [ H.ignoreOnWindows "tx-build-estimate" Cardano.Testnet.Test.Cli.TxBuildEstimate.hprop_tx_build_estimate
        ]

    --, T.testGroup "SubmitApi"
    --    [ T.testGroup "Babbage"
    --        [ H.ignoreOnWindows "transaction" Cardano.Testnet.Test.SubmitApi.Babbage.Transaction.hprop_transaction
    --        ]
    --    ]
    ]

defaultMainWithIngredientsAndOptions :: [T.Ingredient] -> T.OptionSet -> T.TestTree -> IO ()
defaultMainWithIngredientsAndOptions ins opts testTree = do
  T.installSignalHandlers
  parsedOpts <- T.parseOptions ins testTree
  let opts' = opts <> parsedOpts

  case T.tryIngredients ins opts' testTree of
    Nothing -> do
      IO.hPutStrLn IO.stderr
        "No ingredients agreed to run. Something is wrong either with your ingredient set or the options."
      IO.exitFailure
    Just act -> do
      ok <- act
      if ok then IO.exitSuccess else IO.exitFailure

main :: IO ()
main = do
  Crypto.cryptoInit

  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  args <- E.getArgs

  let opts = T.singleOption $ T.NumThreads 1

  E.withArgs args $ tests >>= defaultMainWithIngredientsAndOptions T.defaultIngredients opts
