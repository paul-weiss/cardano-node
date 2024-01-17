{-# LANGUAGE LambdaCase #-}

module Testnet.Aeson (
    assertHasArrayMappingOfLength
  , assertHasKey
  , assertObject
  ) where
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import           GHC.Stack (HasCallStack)
import           Hedgehog (MonadTest)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H

-- | @assertObjectHasArrayMappingOfLength key len obj@ checks that the JSON
-- Objecct @obj@ maps @key@ to a JSON array of size @len@. If not it fails the test.
assertHasArrayMappingOfLength :: ()
  => HasCallStack
  => MonadTest m
  => Text.Text -- ^ The key where the JSON array is expected to be
  -> Int -- ^ The expected length of the array
  -> Aeson.Object -- ^ The JSON to inspect
  -> m ()
assertHasArrayMappingOfLength key len obj =
  case Aeson.lookup (Aeson.fromText key) obj of
    Just (Aeson.Array a) | Vector.length a == len ->
      H.success
    Just (Aeson.Array a) -> do
      H.note_ $ "Expected array at \"" <> Text.unpack key <> "\" to have length " <> show len <> ", but got: " <> show (Vector.length a)
      H.failure
    Just unexpected -> do
      H.note_ $ "Expected a JSON object at \"" <> Text.unpack key <> "\", but got: " <> showAesonKind unexpected
      H.failure
    Nothing -> do
      H.note_ $ "Key not found in JSON object: " <> Text.unpack key
      H.failure

assertHasKey :: ()
  => MonadTest m
  => Aeson.Key
  -> Aeson.Value
  -> m Aeson.Value
assertHasKey k json = do
  obj <- assertObject json
  case Aeson.lookup k obj of
    Nothing -> do
      H.note_ $ "Key not found in JSON object: " <> show k
      H.failure
    Just value ->
      return value

-- | @assertObject json@ returns the underlying @Aeson.Object _@ if any.
-- Otherwise it fails the test.
assertObject :: ()
  => HasCallStack
  => MonadTest m
  => Aeson.Value -- ^ The JSON to inspect
  -> m Aeson.Object
assertObject json =
  case json of
    Aeson.Object o ->
      return o
    _ -> do
      H.note_ $ "Expected a JSON object, but got: " <> showAesonKind json
      H.failure

-- | Pretty print the kind of the given value
showAesonKind :: Aeson.Value -> String
showAesonKind = \case
    Aeson.Array _  -> "array"
    Aeson.Bool _   -> "bool"
    Aeson.Number _ -> "number"
    Aeson.Null     -> "null"
    Aeson.Object _ -> "object"
    Aeson.String _ -> "string"
