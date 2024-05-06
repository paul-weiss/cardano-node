{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.TxGenerator.Setup.NixService
       ( NixServiceOptions (..)
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

import           Data.Aeson as Aeson
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe)
import qualified Data.Time.Clock as Clock (DiffTime, secondsToDiffTime)
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


-- Long GC pauses on target nodes can trigger spurious MVar deadlock
-- detection. Increasing this timeout can help mitigate those errors.
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
