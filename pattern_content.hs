{-# LANGUAGE DeriveDataTypeable #-}
module Main where
 
import CSV
import PML
import Utils
import XML
import Text.Pandoc.Filter.Tables (makeTable)
import Text.Pandoc.Filter.Include (include)

import Data.List (intercalate, nub)
import Data.Maybe (fromMaybe, maybe)
import Formal.PML.Subtree (getSubtree)
import NLP.FullStop (segment)
import System.Console.CmdArgs
import System.FilePath.Posix (takeBaseName, (</>))
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.JSON
import Text.Printf (printf)



-- class "gtm-rec-impl"
formatGTMRecImpl :: Options -> [(String, String)] -> IO Block
formatGTMRecImpl opts namevals =
    let recName = fromMaybe "XXX 'rec' missing XXX" $ lookup "rec" namevals
        model = fromMaybe "XXX 'model' missing XXX" $ lookup "model" namevals
        subtree = fromMaybe "XXX 'subtree' missing XXX"  $ lookup "subtree" namevals
        title = printf "Implementation details for practice %s -- %s" (fromMaybe "XXX 'section' missing XXX" $ lookup "section" namevals) (fromMaybe "XXX 'title' missing XXX" $ lookup "title" namevals)
        infile = (opt_modelsdir opts) </> model
        actions = read $ fromMaybe "XXX 'actions' missing XXX" $ lookup "actions" namevals :: [String] 
    in readFile infile >>= 
                (\pml -> return $ makeTable (title) ["Step", "Description", "Role(s)"] [0.20, 0.60, 0.2] ["l", "l", "l"] $ map (\(n, _, s, agents) -> [wordify n, s, intercalate ", " $   map decamelize agents]) $ getActions pml subtree Nothing)


-- class "pml-stakeholders"
formatStakeholders :: Options -> [(String, String)] -> IO Block
formatStakeholders opts namevals = 
    let model = fromMaybe "XXX 'model' missing XXX"  $ lookup "model" namevals
        subtree = fromMaybe "XXX 'subtree' missing XXX"  $ lookup "subtree" namevals
        infile = (opt_modelsdir opts) </> model
    in readFile infile >>= (\pml -> return $ mkOrderedList $ map decamelize $ getAgents pml subtree)

-- class "pml-inputs" and "pml-outputs"
formatEntryExit :: Options -> IOType -> [(String, String)] -> IO Block
formatEntryExit opts t namevals = 
    let model = fromMaybe "XXX 'model' missing XXX"  $ lookup "model" namevals
        subtree = fromMaybe "XXX 'subtree' missing XXX"  $ lookup "subtree" namevals
        infile = (opt_modelsdir opts) </> model
    in readFile infile >>= 
           (\pml -> return $ mkOrderedList $ map (capitalize . decamelize . (naturalize t)) $ getInOutputs t pml subtree)

-- class "why-doit"
formatWhy :: Options -> [(String, String)] -> IO Block
formatWhy opts namevals = 
    let recName = fromMaybe "XXX 'rec' missing XXX"  $ lookup "rec" namevals
        csvFile = (opt_issuesdir opts) </> "SCAL_Issues.csv"
    in readFile csvFile >>= (\c -> return $ nub $ lookupCSV recName 0 2 $ getCSV ',' c) >>=
           (\rs -> return $ mkOrderedList  rs)


-- class "gtm-rec-desc"
formatGTMRec :: Options -> [(String, String)] -> IO Block
formatGTMRec opts namevals = 
    let recName = fromMaybe "XXX 'rec' missing XXX"  $ lookup "rec" namevals
    in getGTMRec ((opt_gtmdir opts) </> "gtm.xml") recName >>= (\d -> return $ Para [(Str d)] )


formatDiv :: Options -> Block -> IO Block
formatDiv opts _div@(Div (_id, classes, namevals) b) =
    if      elem "gtm-rec-desc" classes then formatGTMRec opts namevals
    else if elem "pml-stakeholders" classes then formatStakeholders opts namevals

    else if elem "pml-inputs" classes  then formatEntryExit opts Input namevals
    else if elem "pml-outputs" classes then formatEntryExit opts Output namevals

    else if elem "why-doit" classes then formatWhy opts namevals
    else if elem "gtm-rec-impl" classes then formatGTMRecImpl opts namevals
    else if elem "include" classes then include _div
    else return $ Plain [(Str ("pattern_content.hs:formatDiv:I got: " ++ _id))]

formatDiv  _ x = return x
    
data Options = Options {
      opt_gtmdir :: String         -- dir with gtm.xml
    , opt_issuesdir :: String      -- dir with issues/SCAL.csv
    , opt_modelsdir :: String      -- dir with pml models
    , opt_safedir :: String        -- dir with safe practices
} deriving (Data, Typeable, Show)
 
options :: Options
options = Options { 
            opt_gtmdir = "../gtm"             &= typ "dir" &= help "directory containing gtm.xml"                  &= name "gtmdir"
          , opt_safedir = "../SAFe_practices" &= typ "dir" &= help "directory containing extracted SAFe practices" &= name "safedir"
          , opt_modelsdir = "../models"       &= typ "dir" &= help "directory containing pml models"               &= name "issuesdir"
          , opt_issuesdir = "../issues"       &= typ "dir" &= help "directory containing issues"                   &= name "pmldir"
                  }
       &= summary "pattern_content (C) John Noll 2016"
       &= program "pattern_content"
 
main :: IO ()
main = do
  opts <- cmdArgs options
  toJSONFilter (formatDiv opts)
