{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE StrictData #-}
module Cardano.Table (module Cardano.Table) where

import Cardano.Prelude
import Data.Text                qualified as T

import Cardano.Util


data Table
  = Props
    { oProps     :: [(Text, Text)]
    , oBody      :: [Table]
    , oConstants :: [(Text, Text)]
    }
  | Table
    { tColHeaders     :: [Text]
    , tExtended       :: Bool
    , tApexHeader     :: Maybe Text
    , tRowHeaders     :: [Text]
    , tColumns        :: [[Text]]
    , tSummaryHeaders :: [Text]
    , tSummaryValues  :: [[Text]]
    , tFormula        :: [Text]
    , tConstants      :: [(Text, Text)]
    }
  deriving (Show)

renderAsLaTeX :: Table -> [Text]

renderAsLaTeX Props{..} =
  ["\\def\\" <> c <> "{" <> x <> "}" | (c, x) <- oProps <> oConstants]
  <>
  (oBody <&> renderAsLaTeX & mconcat)
renderAsLaTeX Table{..} =
  ["\\begin{tabular}{l" <> T.replicate (length tColHeaders) "|r" <> "}"]
  <>
  [T.intercalate " & " (map latexFixup row) <> " \\\\" | row <- rowSet]
  <>
  [T.intercalate " & " (map latexFixup summary) <> " \\\\" | summary <- summarySet]
  <>
  ["\\end{tabular}"] where
  rowSet = ("":tColHeaders) : transpose (tRowHeaders : tColumns)
  summarySet = transpose $ tSummaryHeaders : tSummaryValues

latexFixup :: Text -> Text
latexFixup = T.replace "%" "\\%"
           . T.replace "&" "\\&"
           . T.replace "#" "\\#"
           . T.replace "_" "\\_"

renderAsOrg :: Table -> [Text]

renderAsOrg Props{..} =
  ((oProps <> [ ("CONSTANTS", T.intercalate " " $ oConstants <&> \(c, x) -> c <> "=" <> x)
              | not (null oConstants)])
   <&> \(name, value) -> mconcat ["#+", name, ": ", value])
  <>
  (oBody <&> renderAsOrg & mconcat)

renderAsOrg Table{tConstants = _:_, tExtended = False} =
  error "Asked to render a non-extended Org table with an extended table feature:  named constants"

renderAsOrg Table{..} =
    renderTableHLine
  : renderTableRow jusAllHeaders
  : renderTableHLine
  : fmap renderTableRow (transpose jusAllColumns)
  & flip (<>)
    jusAllSummaryLines
  & flip (<>)
    jusAllConstantLines
  & flip (<>)
    (bool [ "#+TBLFM:" <> (tFormula & T.intercalate "::") ] [] (null tFormula))
 where
   jusAllHeaders :: [Text]
   jusAllHeaders = zipWith (`T.justifyRight` ' ')
                           (rowHdrWidth : colWidths)
                           (fromMaybe " " tApexHeader : tColHeaders)
                   & consIfSpecial "!"

   jusAllColumns :: [[Text]]
   jusAllColumns = (tRowHeaders <&> T.justifyRight rowHdrWidth ' ')
                   : justifySourceColumns tColumns
                   & consIfSpecial specialCol

   jusAllSummaryLines :: [Text]
   jusAllSummaryLines =
     if null tSummaryHeaders then [] else
       renderTableHLine :
       fmap renderTableRow (zipWith (:)
                            (tSummaryHeaders <&> T.justifyRight rowHdrWidth ' ')
                            (transpose (justifySourceColumns tSummaryValues))
                             <&> consIfSpecial (bool " " "#" tExtended))

   justifySourceColumns :: [[Text]] -> [[Text]]
   justifySourceColumns = zipWith (\w-> fmap (T.justifyRight w ' ')) colWidths

   jusAllConstantLines :: [Text]
   jusAllConstantLines =
     if null tConstants then [] else
       renderTableHLine :
       fmap renderTableRow (zipWith (:)
                             (cycle ["_", "#"])
                             constRows)
    where
      constRows = (chunksOf nTotalColumns tConstants                                          -- we can fit so many definitions per row
                   & mapLast (\row -> row <> replicate (nTotalColumns - length row) ("", "")) -- last row needs completion
                   & fmap (`zip` allColWidths))                                               -- and we supply column widths for justification
                  <&> transpose . fmap (\((name, value), w) ->                                -- each row -> row pair of justified [Name, Definition]
                                          [ T.justifyRight w ' ' name
                                          , T.justifyRight w ' ' value])
                   & concat                                                                   -- merge into a single list of rows

   rowHdrWidth, nTotalColumns :: Int
   rowHdrWidth = maximum $ length <$> (maybeToList tApexHeader
                                       <> tRowHeaders
                                       <> tSummaryHeaders)
   nTotalColumns = length allColWidths

   colWidths, allColWidths :: [Int]
   allColWidths = rowHdrWidth : colWidths
   colWidths = maximum . fmap length <$>
               (tColumns
                & zipWith (:)  tColHeaders
                & if null tSummaryValues then identity
                  else zipWith (<>) tSummaryValues)

   specialCol :: [Text]
   specialCol = length tRowHeaders `replicate` "#"

   consIfSpecial :: a -> [a] -> [a]
   consIfSpecial x = bool  identity        (x:)  tExtended

   renderTableRow :: [Text] -> Text
   renderTableRow xs = "| " <> T.intercalate " | " xs <> " |"
   renderTableHLine :: Text
   renderTableHLine = ("|-" <>) . (<> "-|") . T.intercalate "-+-" . (flip T.replicate "-" <$>) $
                      rowHdrWidth
                      : colWidths
                      & consIfSpecial 1
