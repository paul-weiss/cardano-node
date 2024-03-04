{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Report
  ( module Cardano.Report
  )
where

import Cardano.Prelude

import Data.Aeson.Encode.Pretty qualified as AEP
import Data.Aeson           qualified as AE
import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List            qualified as List
import Data.Map.Strict      qualified as Map
import Data.Text            qualified as T
import Data.Text.Lazy       qualified as LT
import Data.Time.Clock
import Data.Time.Format
import System.Posix.User
import System.Environment (lookupEnv)

import Text.EDE hiding (Id)

import Data.CDF
import Data.Tuple.Extra (fst3)
import Cardano.Render
import Cardano.Util
import Cardano.Analysis.API
import Cardano.Analysis.Summary

type (&) :: (k -> Constraint) -> (k -> Constraint) -> (k -> Constraint)
class    (cls a, cls1 a) => (cls & cls1) a
instance (cls a, cls1 a) => (cls & cls1) a


newtype Author   = Author   { unAuthor   :: Text } deriving newtype (FromJSON, ToJSON)
newtype ShortId  = ShortId  { unShortId  :: Text } deriving newtype (FromJSON, ToJSON)
newtype Tag      = Tag      { unTag      :: Text } deriving newtype (FromJSON, ToJSON)

data ReportMeta
  = ReportMeta
    { rmAuthor       :: !Author
    , rmDate         :: !Text
    , rmLocliVersion :: !LocliVersion
    , rmTarget       :: !Version
    , rmTag          :: !Tag
    }
instance ToJSON ReportMeta where
  toJSON ReportMeta{..} = object
    [ "author"     .= rmAuthor
    , "date"       .= rmDate
    , "locli"      .= rmLocliVersion
    , "target"     .= rmTarget
    , "tag"        .= rmTag
    ]

getReport :: [Metadata] -> Version -> IO ReportMeta
getReport metas _ver = do
  rmAuthor <- getGecosFullUsername
              `catch`
              \(_ :: SomeException) ->
                 getFallbackUserId
  rmDate <- getCurrentTime <&> T.take 16 . show
  let rmLocliVersion = getLocliVersion
      rmTarget = Version $ ident $ last metas
      rmTag = Tag $ multiRunTag Nothing metas
  pure ReportMeta{..}
 where
   getGecosFullUsername, getFallbackUserId :: IO Author
   getGecosFullUsername =
     (getUserEntryForID =<< getRealUserID)
     <&> Author . T.pack . takeWhile (/= ',') . userGecos

   getFallbackUserId =
     (\user host->
        Author . T.pack $
        fromMaybe "user" user <> "@" <> fromMaybe "localhost" host)
     <$> lookupEnv "USER"
     <*> lookupEnv "HOSTNAME"

data Workload
  = WValue
  | WPlutusLoopCountdown
  | WPlutusLoopSECP
  | WPlutusUnknown

instance ToJSON Workload where
  toJSON = \case
    WValue               -> "value-only"
    WPlutusLoopCountdown -> "Plutus countdown loop"
    WPlutusLoopSECP      -> "Plutus SECP loop"
    WPlutusUnknown       -> "Plutus (other)"

filenameInfix :: Workload -> Text
filenameInfix = \case
  WPlutusLoopCountdown  -> "plutus"
  WPlutusLoopSECP       -> "plutus-secp"
  WValue                -> "value-only"
  _                     -> "unknown"

data Section a p where
  STable ::
    { sData      :: !(a p)
    , sFields    :: !FSelect
    , sNameCol   :: !Text
    , sValueCol  :: !Text
    , sDataRef   :: !Text
    , sOrgFile   :: !Text
    , sTitle     :: !Text
    } -> Section a p

formatSuffix :: RenderFormat -> Text
formatSuffix AsOrg = "org"
formatSuffix AsLaTeX = "latex"
formatSuffix _ = "txt"

summaryReportSection :: RenderFormat -> Summary f -> Section Summary f
summaryReportSection rf summ =
  STable summ (ISel @SummaryOne $ iFields sumFieldsReport) "Parameter" "Value"   "summary"
    ("summary." <> formatSuffix rf)
    "Overall run parameters"

data AmbiguousSection = forall p a . CDFFields p a =>
  AmbiguousSection (Section p a)

-- analysesReportSections :: RenderFormat -> MachPerf (CDF I) -> BlockProp f -> [Section MachPerf (CDF I)]
analysesReportSections :: RenderFormat -> MachPerf (CDF I) -> BlockProp f -> [AmbiguousSection]
analysesReportSections rf mp bp =
  [ AmbiguousSection $ STable mp (DSel @MachPerf  $ dFields mtFieldsReport)   "metric"  "average"    "perf"
    ("clusterperf.report." <> formatSuffix rf)
    "Resource Usage"

  , AmbiguousSection $ STable bp (DSel @BlockProp $ dFields bpFieldsControl)  "metric"  "average" "control"
    ("blockprop.control." <> formatSuffix rf)
    "Anomaly control"

  , AmbiguousSection $ STable bp (DSel @BlockProp $ dFields bpFieldsForger)   "metric"  "average"   "forge"
    ("blockprop.forger." <> formatSuffix rf)
    "Forging"

  , AmbiguousSection $ STable bp (DSel @BlockProp $ dFields bpFieldsPeers)    "metric"  "average"   "peers"
    ("blockprop.peers." <> formatSuffix rf)
    "Individual peer propagation"

  , AmbiguousSection $ STable bp (DSel @BlockProp $ dFields bpFieldsEndToEnd) "metric"  "average" "end2end"
    ("blockprop.endtoend." <> formatSuffix rf)
    "End-to-end propagation"
  ]

--
-- Representation of a run, structured for template generator's needs.
--

liftTmplRun :: Summary a -> TmplRun
liftTmplRun Summary{sumWorkload=generatorProfile
                   ,sumMeta=meta@Metadata{..}} =
  TmplRun
  { trMeta      = meta
  , trManifest  = manifest & unsafeShortenManifest 5
  , trWorkload  =
    case plutusLoopScript generatorProfile of
      Nothing                               -> WValue
      Just script
        | script == "Loop"                  -> WPlutusLoopCountdown
        | script == "EcdsaSecp256k1Loop"    -> WPlutusLoopSECP
        | script == "SchnorrSecp256k1Loop"  -> WPlutusLoopSECP
        | otherwise                         -> WPlutusUnknown
  }

data TmplRun
  = TmplRun
    { trMeta         :: !Metadata
    , trWorkload     :: !Workload
    , trManifest     :: !Manifest
    }

instance ToJSON TmplRun where
  toJSON TmplRun{..} =
    object
      [ "meta"       .= trMeta
      , "workload"   .= trWorkload
      , "branch"     .= componentBranch (getComponent "cardano-node" trManifest)
      , "ver"        .= ident trMeta
      , "rev"        .= unManifest trManifest
      , "fileInfix"  .= filenameInfix trWorkload
      ]

data LaTeXSection =
  LaTeXSection {
      latexSectionTitle   :: Text
    , latexSectionColumns :: [Text]
    , latexSectionRows    :: [Text]
    , latexSectionData    :: [[Text]]
  } deriving (Eq, Read, Show)

liftTmplLaTeX :: KnownCDF a => Section (CDF a) Double -> LaTeXSection
liftTmplLaTeX STable {..} =
  LaTeXSection {
      latexSectionTitle   = sTitle
    -- *** XXX MAJOR BOGON XXX ***
    -- The column labels need to be associated with groups of columns.
    , latexSectionColumns = case sFields of
                                  ISel sel -> map fShortDesc (filter sel timelineFields)
                                  DSel sel -> map fShortDesc (filter sel      cdfFields)
    , latexSectionRows    = rows
    -- *** XXX MAJOR BOGON XXX ***
    -- Rows need to be properly extracted from the CDF/MachPerf/etc.
    , latexSectionData    = [concat [let range = cdfRange stats in map (formatDouble width) [cdfMedian stats, cdfAverageVal stats, cdfStddev stats, low range, high range] | width <- widths] | stats <- [sData]]
    }
    where
          -- *** XXX MAJOR BOGON XXX ***
          -- Row labels need to get prepended to the table's rows.
          rows = repeat ""
          widths = case sFields of
                           ISel sel -> [fWidth | Field {..} <- filter sel timelineFields]
                           DSel sel -> [fWidth | Field {..} <- filter sel      cdfFields]

liftTmplSection :: Section a p -> TmplSection
liftTmplSection =
  \case
    STable{..} ->
      TmplTable
      { tsTitle       = sTitle
      , tsNameCol     = sNameCol
      , tsValueCol    = sValueCol
      , tsDataRef     = sDataRef
      , tsOrgFile     = sOrgFile
      , tsRowPrecs    = fs <&> fromEnum
      , tsVars        = [ ("nSamples", "Sample count")
                        ]
      }
     where fs = case sFields of
                  ISel sel -> filter sel timelineFields <&> fPrecision
                  DSel sel -> filter sel      cdfFields <&> fPrecision

liftTmplSection' :: AmbiguousSection -> TmplSection
liftTmplSection' (AmbiguousSection section) =
  liftTmplSection section

data TmplSection
  = TmplTable
    { tsTitle        :: !Text
    , tsNameCol      :: !Text
    , tsValueCol     :: !Text
    , tsDataRef      :: !Text
    , tsOrgFile      :: !Text
    , tsRowPrecs     :: ![Int]
    , tsVars         :: ![(Text, Text)] -- map from Org constant name to description
    }

instance ToJSON TmplSection where
  toJSON TmplTable{..} = object
    [ "title"     .= tsTitle
    , "nameCol"   .= tsNameCol
    , "valueCol"  .= tsValueCol
    , "dataRef"   .= tsDataRef
    , "orgFile"   .= tsOrgFile
    -- Yes, strange as it is, this is the encoding to ease iteration in ED-E.
    , "rowPrecs"  .= tsRowPrecs
    , "vars"      .= Map.fromList (zip tsVars ([0..] <&> flip T.replicate ">" . (length tsVars -))
                                   <&> \((k, name), angles) ->
                                         (k, Map.fromList @Text
                                             [("name", name),
                                              ("angles", angles)]))
    ]

generate' :: (SomeSummary (KnownCDF & AE.ToJSON1), ClusterPerf, SomeBlockProp)
          -> [(SomeSummary (KnownCDF & AE.ToJSON1), ClusterPerf, SomeBlockProp)]
          -- summary, resource, anomaly, forging, peers
          -> IO (Text, Text, Text, Text, Text, Text)
generate' baseline@(SomeSummary (summ :: Summary f), cp :: cpt, SomeBlockProp (bp :: BlockProp bpt)) rest = do
  ctx <- getReport metas (last restTmpls & trManifest & getComponent "cardano-node" & ciVersion)
  time <- getCurrentTime
  _ <- pure $ mkTmplEnv ctx (liftTmplRun summ) $ fmap ((\(SomeSummary ss) -> liftTmplRun ss). fst3) rest
  let anchorForRendering =
          Anchor {
              aRuns = getName summ : map (\(SomeSummary ss, _, _) -> getName ss) rest
             -- *** XXX MAJOR BOGON XXX ***
             -- Filters, slots and blocks should actually come from somewhere.
            , aFilters = ([], [])
            , aSlots = Nothing
            , aBlocks = Nothing
            , aVersion = getLocliVersion
            , aWhen = time
          }
          where
            getName = tag . sumMeta
  _ <- pure ( anomalyRendering anchorForRendering
            , forgingRendering anchorForRendering
            , peersRendering anchorForRendering
            , resourceRendering anchorForRendering
            )
  pure (titlingText ctx, summaryText, resourceText, anomalyText, forgingText, peersText)
  where
   -- *** XXX DUMP THESE THINGS XXX ***
   -- Move over to the Table API
   resourceText = unlines resourceLines
   anomalyText = unlines anomalyLines
   forgingText = unlines forgingLines
   peersText = unlines peersLines
   fmtTime = T.pack . formatTime defaultTimeLocale rfc822DateFormat
   metas :: [Metadata]
   metas = sumMeta summ : fmap (\(SomeSummary ss, _, _) -> sumMeta ss) rest
   tags :: [Text]
   tags = map tag metas
   summaryText :: Text
   summaryText = unlines summaryLines
   summaryFields :: [Text]
   summaryFields = [sd | fld@Field { fShortDesc = sd }
                                       :: Field ISelect I (Summary f)
                                       <- timelineFields
                       , iFields sumFieldsReport fld]
   -- *** XXX MAJOR BOGON XXX ***
   -- Summary fields vary in type and can't all be properly rendered
   -- as times. Many should be strings.
   summaryLines :: [Text]
   summaryLines =
     [ "\\begin{tabular}{c" <> List.foldr1 (<>) (map (const "|r") metas) <> "}"]
        ++ map ((<> "\\\\") . List.foldr1 (<>) . intersperse " & ")
           ( ("" : tags)
           : zipWith (:) summaryFields [map mkTime (baseline : rest)]
           )
        ++ ["\\end{tabular}"]
   -- *** XXX MAJOR BOGON XXX ***
   -- This should come from somewhere, save rcFormat.
   renderConfig =
     RenderConfig {
       rcFormat = AsLaTeX
     , rcDateVerMetadata = False
     , rcRunMetadata = False
     }
   anomalyRendering, forgingRendering, peersRendering, resourceRendering :: Anchor -> [(Text, [Text])]
   anomalyRendering anchor = renderAnalysisCDFs anchor (dFields bpFieldsControl) OfInterCDF Nothing renderConfig bp
   forgingRendering anchor = renderAnalysisCDFs anchor (dFields bpFieldsForger) OfInterCDF Nothing renderConfig bp
   peersRendering anchor = renderAnalysisCDFs anchor (dFields bpFieldsPeers) OfInterCDF Nothing renderConfig bp
   resourceRendering anchor = renderAnalysisCDFs anchor (dFields mtFieldsReport) OfInterCDF Nothing renderConfig cp
   mkTime :: (SomeSummary (KnownCDF & AE.ToJSON1), t', t'') -> Text
   mkTime (SomeSummary x, _, _) = fmtTime $ sumAnalysisTime x
   restTmpls = fmap ((\(SomeSummary ss) -> liftTmplRun ss) . fst3) rest
   anomalyLines :: [Text]
   anomalyLines = (mkLines :: ((Field DSelect p BlockProp) -> Bool) -> [Text]) (dFields bpFieldsControl)
   forgingLines :: [Text]
   forgingLines = (mkLines :: ((Field DSelect p BlockProp) -> Bool) -> [Text]) (dFields bpFieldsForger)
   peersLines :: [Text]
   peersLines = (mkLines :: ((Field DSelect p BlockProp) -> Bool) -> [Text]) (dFields bpFieldsPeers)
   resourceLines :: [Text]
   resourceLines = (mkLines :: ((Field DSelect p MachPerf) -> Bool) -> [Text]) (dFields mtFieldsReport)
   -- *** XXX MAJOR BOGON XXX ***
   -- Fields should vary between percentages and timings, not
   -- all be timings. Worse yet, each benchmark run should correspond to
   -- means, variances, ranges etc. not just single numbers.
   mkLines :: CDFFields cstr p => (Field DSelect p cstr -> Bool) -> [Text]
   mkLines selector =
     [ "\\begin{tabular}{c" <> List.foldr1 (<>) (map (const "|r") fields) <> "}"]
        ++ map ((<> "\\\\") . List.foldr1 (<>) . intersperse " & ")
           ( ("" : tags)
           : zipWith (:) fields [map mkTime (baseline : rest)]
           )
        ++ ["\\end{tabular}"]
     where
       fields = map fShortDesc $ filter selector cdfFields
   summarySection = summaryReportSection AsLaTeX summ
   reportSections = analysesReportSections AsLaTeX cp bp
   -- Authors should have "\\and" interspersed between them in LaTeX.
   -- Write this out to titling.latex
   titlingText ctx = unlines
     $ [ "\\def\\@locliauthor{" <> unAuthor (rmAuthor ctx) <> "}"
       , "\\def\\@loclititle{Value Workload for " <> unTag (rmTag ctx) <> "}"
       , "\\def\\@loclidate{" <> rmDate ctx <> "}"
       ]
   mkTmplEnv rc b rs = fromPairs
     [ "report"     .= rc
     , "base"       .= b
     , "runs"       .= rs
     , "summary"    .= liftTmplSection summarySection
     , "analyses"   .= fmap liftTmplSection' reportSections
     , "dictionary" .= metricDictionary
     , "charts"     .=
       ((dClusterPerf metricDictionary & onlyKeys clusterPerfKeys)
        <>
        (dBlockProp   metricDictionary & onlyKeys blockPropKeys))
     ]

generate :: InputDir -> Maybe TextInputFile
         -> (SomeSummary KnownCDF, ClusterPerf, SomeBlockProp) -> [(SomeSummary KnownCDF, ClusterPerf, SomeBlockProp)]
         -> IO (ByteString, ByteString, Text)
generate (InputDir ede) mReport (SomeSummary summ, cp, SomeBlockProp bp) rest = do
  ctx  <- getReport metas (last restTmpls & trManifest & getComponent "cardano-node" & ciVersion)
  tmplRaw <- BS.readFile (maybe defaultReportPath unTextInputFile mReport)
  tmpl <- parseWith defaultSyntax (includeFile ede) "report" tmplRaw
  let tmplEnv           = mkTmplEnv ctx baseTmpl restTmpls
      tmplEnvSerialised = AEP.encodePretty tmplEnv
  Text.EDE.result
    (error . show)
    (pure . (tmplRaw, LBS.toStrict tmplEnvSerialised,) . LT.toStrict) $ tmpl >>=
    \x ->
      renderWith mempty x tmplEnv
 where
   metas = sumMeta summ : fmap (\(SomeSummary ss, _, _) -> sumMeta ss) rest

   defaultReportPath = ede <> "/report.ede"

   baseTmpl  = liftTmplRun summ
   restTmpls = fmap ((\(SomeSummary ss) -> liftTmplRun ss). fst3) rest

   mkTmplEnv rc b rs = fromPairs
     [ "report"     .= rc
     , "base"       .= b
     , "runs"       .= rs
     , "summary"    .= liftTmplSection (summaryReportSection AsOrg summ)
     , "analyses"   .= (liftTmplSection' <$> analysesReportSections AsOrg cp bp)
     , "dictionary" .= metricDictionary
     , "charts"     .=
       ((dClusterPerf metricDictionary & onlyKeys clusterPerfKeys)
        <>
        (dBlockProp   metricDictionary & onlyKeys blockPropKeys))
     ]

onlyKeys :: [Text] -> Map.Map Text DictEntry -> [DictEntry]
onlyKeys ks m =
  ks <&>
     \case
       (Nothing, k) -> error $ "Report.generate:  missing metric: " <> show k
       (Just x, _) -> x
     . (flip Map.lookup m &&& identity)

blockPropKeys, clusterPerfKeys :: [Text]
clusterPerfKeys =
          [ "CentiCpu"
          , "CentiGC"
          , "CentiMut"
          , "Alloc"
          , "GcsMajor"
          , "GcsMinor"
          , "Heap"
          , "Live"
          , "RSS"

          , "cdfStarted"
          , "cdfBlkCtx"
          , "cdfLgrState"
          , "cdfLgrView"
          , "cdfLeading"

          , "cdfDensity"
          , "cdfBlockGap"
          , "cdfSpanLensCpu"
          , "cdfSpanLensCpuEpoch"
          ]

blockPropKeys =
          [ "cdfForgerLead"
          , "cdfForgerTicked"
          , "cdfForgerMemSnap"
          , "cdfForgerForge"
          , "cdfForgerAnnounce"
          , "cdfForgerSend"
          , "cdfPeerNoticeFirst"
          , "cdfPeerAdoption"
          , "cdf0.50"
          , "cdf0.80"
          , "cdf0.90"
          , "cdf0.96"
          ]
