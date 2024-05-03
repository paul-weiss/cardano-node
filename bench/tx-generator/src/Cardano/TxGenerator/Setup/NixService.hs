{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: remove me
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Cardano.TxGenerator.Setup.NixService
       ( NixServiceOptions (..)
       --, NodeConfigDiffTime (..)
       , WithAlias(..)
       , getKeepaliveTimeout
       , getAliasPayload
       , getNodeConfigFile
       , setNodeConfigFile
       , txGenTxParams
       , txGenConfig
       , txGenPlutusParams
       )
       where

import           Cardano.Api (AnyCardanoEra, mapFile)

import           Cardano.CLI.Types.Common (FileDirection (..), SigningKeyFile)
import qualified Cardano.Ledger.Coin as L
import           Cardano.Node.Configuration.NodeAddress (NodeIPv4Address)
import           Cardano.Node.Types (AdjustFilePaths (..))
import           Cardano.TxGenerator.Internal.Orphans ()
import           Cardano.TxGenerator.Types

import qualified Control.Applicative as App (empty)
import           Data.Aeson as Aeson
-- import qualified Data.Aeson as Aeson (Options, defaultOptions, genericParseJSON, withObject)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe)
import           Data.Scientific (Scientific)
import qualified Data.Time.Clock as Clock (DiffTime, diffTimeToPicoseconds, secondsToDiffTime)
import           GHC.Generics (Generic)


data NixServiceOptions = NixServiceOptions {
    _nix_debugMode        :: Bool
  , _nix_tx_count         :: NumberOfTxs
  , _nix_tps              :: TPSRate
  , _nix_inputs_per_tx    :: NumberOfInputsPerTx
  , _nix_outputs_per_tx   :: NumberOfOutputsPerTx
  , _nix_tx_fee           :: L.Coin
  , _nix_min_utxo_value   :: L.Coin
  , _nix_add_tx_size      :: TxAdditionalSize
  , _nix_init_cooldown    :: Double
  , _nix_era              :: AnyCardanoEra
  , _nix_plutus           :: Maybe TxGenPlutusParams
  , _nix_keepalive        :: Maybe Integer
  , _nix_nodeConfigFile       :: Maybe FilePath
  , _nix_cardanoTracerSocket  :: Maybe FilePath
  , _nix_sigKey               :: SigningKeyFile In
  , _nix_localNodeSocketPath  :: String
  , _nix_targetNodes          :: NonEmpty (WithAlias NodeIPv4Address)
  } deriving (Show, Eq)

deriving instance Generic NixServiceOptions

{-
newtype NodeConfigDiffTime =
  NodeConfigDiffTime { nodeConfigDiffTime :: Clock.DiffTime }
  deriving (Eq, Ord, Read, Show)

instance FromJSON NodeConfigDiffTime where
  omittedField = Just $
    NodeConfigDiffTime { nodeConfigDiffTime = Clock.secondsToDiffTime 10 }
  parseJSON = \case
    Number (scientificNumber :: Scientific) ->
      -- DiffTime uses fixed-point picoseconds.
      let nodeConfigDiffTime :: Clock.DiffTime =
            Clock.secondsToDiffTime $ round scientificNumber
       in pure $ NodeConfigDiffTime {..}
    _                       -> App.empty -- this fails

instance ToJSON NodeConfigDiffTime where
  omitField NodeConfigDiffTime {..} = nodeConfigDiffTime == 10
  toJSON NodeConfigDiffTime {..} =
    let picoSeconds = Clock.diffTimeToPicoseconds nodeConfigDiffTime
     in toJSON $ picoSeconds `div` 10^(12 :: Int)
-}

-- only works on JSON Object types
data WithAlias a =
  WithAlias String a

deriving instance Show a    => Show    (WithAlias a)
deriving instance Eq   a    => Eq      (WithAlias a)
-- deriving instance Generic a => Generic (WithAlias a)

-- { "alias": "foo", "addr": ..., "port": ... }
instance (Show a, FromJSON a) => FromJSON (WithAlias a) where
  parseJSON val = case fromJSON val of
      Error e          -> fail e
      Success payload  -> withObject "WithAlias" (\o' -> do
        alias <- o' .:? "name" .!= show payload
        pure $ WithAlias alias payload
        ) val

instance ToJSON a => ToJSON (WithAlias a) where
  toJSON (WithAlias _ val) = toJSON val


-- TODO: add comment about hard-coded default value
-- and when to change (account for long GC pauses at target nodes)
getKeepaliveTimeout :: NixServiceOptions -> Clock.DiffTime
getKeepaliveTimeout = maybe 10 Clock.secondsToDiffTime . _nix_keepalive

getAliasPayload :: WithAlias a -> a
getAliasPayload (WithAlias _ val) = val

getNodeConfigFile :: NixServiceOptions -> Maybe FilePath
getNodeConfigFile = _nix_nodeConfigFile

setNodeConfigFile :: NixServiceOptions -> FilePath -> NixServiceOptions
setNodeConfigFile opts filePath = opts {_nix_nodeConfigFile = Just filePath }

-- dropping the '_nix_ prefix of above Haskell ADT field labels is assumed
-- to match JSON attribute names as provided by the Nix service definition
jsonOptions :: Aeson.Options
jsonOptions = Aeson.defaultOptions { fieldLabelModifier = drop 5 }

instance FromJSON NixServiceOptions where
  parseJSON = Aeson.genericParseJSON jsonOptions

instance AdjustFilePaths NixServiceOptions where
  adjustFilePaths f opts
    = opts {
      _nix_nodeConfigFile = f <$> _nix_nodeConfigFile opts
    , _nix_sigKey = mapFile f $ _nix_sigKey opts
    }


-- | This deserialization is not a general one for that type, but custom-tailored
--   to the service definition in: nix/nixos/tx-generator-service.nix
instance FromJSON TxGenPlutusParams where
  parseJSON = Aeson.withObject "TxGenPlutusParams" $ \o ->
    PlutusOn
      <$> o .: "type"
      <*> o .: "script"
      <*> o .:? "datum"
      <*> o .:? "redeemer"
      <*> o .:? "limitExecutionMem"
      <*> o .:? "limitExecutionSteps"


---- mapping of Nix service options to API types

txGenTxParams :: NixServiceOptions -> TxGenTxParams
txGenTxParams NixServiceOptions{..}
  = TxGenTxParams {
    txParamFee = _nix_tx_fee
  , txParamAddTxSize = _nix_add_tx_size
  , txParamTTL = txParamTTL defaultTxGenTxParams
  }

txGenConfig :: NixServiceOptions -> TxGenConfig
txGenConfig NixServiceOptions{..}
  = TxGenConfig {
    confMinUtxoValue = _nix_min_utxo_value
  , confTxsPerSecond = _nix_tps
  , confInitCooldown = _nix_init_cooldown
  , confTxsInputs = _nix_inputs_per_tx
  , confTxsOutputs = _nix_outputs_per_tx
  }

txGenPlutusParams :: NixServiceOptions -> TxGenPlutusParams
txGenPlutusParams
  = fromMaybe PlutusOff . _nix_plutus
