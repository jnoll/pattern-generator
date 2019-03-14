{-# LANGUAGE DeriveGeneric #-} -- Allows automatic derivation of From/ToJSON
{-# LANGUAGE DeriveDataTypeable #-}
module Main where
 
import CSV (getCSV)
import XML (getGTMRec, getGTMGrandParentName, getGTMParentName)
import PML (getPuml, gtmGraphOptions, modelGraphOptions)
import Formal.PML.GraphOptions (GraphOptions(..), GraphType(..))
import Utils (fromSpace, stringify, noDots, noCommas)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import System.Console.CmdArgs
import Text.Printf (printf)
import Text.StringTemplate
import Text.Pandoc.Shared (trim) -- overkill, but Pandoc is included anyway
import System.FilePath (takeFileName, (</>), (<.>))
import Data.Yaml          -- for json conversion, which is really yaml
import GHC.Generics       -- for to/from json 

newActionColor :: String
newActionColor = "GreenYellow"

changedActionColor :: String
changedActionColor = "thistle"

data ComponentType = Pat | Sec
                   deriving Eq;

printDep :: ComponentType -> String -> String  -> IO ()
printDep ct sec outbase = do
  let imgfile = outbase <.> "png"
      pumlfile = outbase <.> "puml"
      secfile = sec <.> "yaml"
      parseOpt = if ct == Sec then "--section" else ""
  putStr $ printf "%s: %s\n\t$(GEN_PUML) %s $<\n" pumlfile secfile parseOpt 

  
printImgDep :: String -> String -> String -> IO ()
printImgDep patName outbase viewport= do
  let imgfile = outbase ++ viewport <.> "png"
      patfile = patName <.> "pdf"
  putStr $ printf "%s: %s\n" patfile imgfile

printImgRule :: String -> String -> IO ()
printImgRule outbase viewport = do
  let imgfile = outbase ++ viewport <.> "png"
      pumlfile = outbase <.> "puml"
  putStr $ printf "%s: %s\n\tjava -jar ${PLANTUML.jar} $<\n\tmv $(basename $(<F)).png tmp.png\n\tconvert -crop %s tmp.png $@\n" imgfile pumlfile viewport
  
-- $Rec$-$AsIsModel$.png, from AsIsModel, AsIsSubtree, AsIsActions
-- Equivalent to pml-graphit --subtree AsIsSubtree --actions [AsIsActions] --depth 2 $AsIsModel$.pml
mkModel :: Options -> Maybe String -> String -> String -> Image -> IO ()
mkModel opts patName secname color image = 
    let model'   = model image
        subtree' = fromMaybe "root" $ subtree image
        actions' = map (\w -> (w, color)) $ fromMaybe [] $ actions image
        modelfile = (takeFileName model')
        outbase = noDots $ printf "%s-%s-%s" secname modelfile subtree' 
        outfile = outbase <.> "puml"
        infile  = (opt_modelsdir opts) </> model' <.> "pml"
        swimlanes' = fromMaybe [] $ swimlanes image 
    in if opt_depends opts then do
      if isJust patName then do
        printDep Sec (secname) outbase 
        if isJust (viewport image) then do
          printImgDep (fromJust patName) outbase (fromJust $ viewport image)
          printImgRule outbase (fromJust $ viewport image)
          else
          printImgDep (fromJust patName) outbase ""
        else 
        printDep Sec secname outbase 
       else (readFile infile) >>= \pml -> writeFile outfile $ getPuml pml subtree' modelGraphOptions { gopt_color = actions', gopt_textwidth = (opt_scriptwidth opts), gopt_scriptwords = (opt_scriptwords opts), gopt_swimlanes = swimlanes', gopt_titleprefix = if subtree' == "root" then Nothing else Just modelfile }

mkAsisModel opts patName sec =
  mapM_ (mkModel opts patName (practice sec) changedActionColor) (images $ asIs sec)

mkTobeModel opts patName sec =
  mapM_ (mkModel opts patName (practice sec) newActionColor) (images $ toBe sec)

  

-- gtm-goal-subtree-$Rec$.png
mkGoalSubtree :: Options -> Pattern -> IO ()
mkGoalSubtree opts pat = 
    let gtmdir = opt_gtmdir opts
        patName = pat_name pat
    in (getGTMGrandParentName (gtmdir ++ "/gtm.xml") $ take 1 patName) >>=
                (\grandp -> let outbase = printf "%s-gtm-goal-subtree" patName
                                outfile = outbase <.> "puml"
                                subtree = printf "SG_%s" $ map fromSpace grandp
                                expand = printf "SubPrac_%s" $ take 1 patName
                                actions = [(printf "Rec_%s" $ patName, "pink"), (expand, "lightgrey")]
                            in if opt_depends opts then do
                              printDep Pat (pat_name pat) outbase
                              printImgDep (pat_name pat) outbase ""
                               else (readFile $ gtmdir </> "gtm.pml") >>= 
                                    \pml -> writeFile outfile $ getPuml pml subtree gtmGraphOptions { gopt_color = actions, gopt_expand = expand })

-- gtm-subprac-subtree-$Rec$.png
mkSubpracSubtree :: Options -> Pattern -> IO ()
mkSubpracSubtree opts pat = 
    let gtmdir = opt_gtmdir opts
        patName = pat_name pat
        outbase = printf "%s-gtm-subprac-subtree" patName
        outfile = outbase <.> "puml"
        subtree = printf "SubPrac_%s"  $ take 1 patName 
        actions = [(printf "Rec_%s" patName, "pink")]
    in if opt_depends opts then  do
      printDep Pat (pat_name pat) outbase 
      printImgDep (pat_name pat) outbase ""
       else (readFile $ gtmdir </> "gtm.pml") >>= 
                \pml -> writeFile outfile $ getPuml pml subtree gtmGraphOptions { gopt_color = actions, gopt_prunedepth = 3 }


-- ${Rec}.md
mkPattern :: Options -> (STGroup String) -> Pattern -> [Section] -> IO ()
mkPattern opts tgrp pat sections = do
  let outbase = (pat_name pat) 
      outfile = outbase <.> "md"
      patName = pat_name pat
      defaultTmpl = fromJust $ getStringTemplate "pattern" tgrp
      tmpl = fromMaybe defaultTmpl $ getStringTemplate outbase tgrp
      sections' = map (mkSection opts pat tgrp) $ sections
    in if opt_depends opts then putStr $ printf "%s: %s\n" (outbase <.> "pdf") outfile
       else getGTMRec ((opt_gtmdir opts) </> "gtm.xml") patName >>= \desc ->
         writeFile outfile $ toString $ setManyAttrib [ ("Rec", patName)
                                                        , ("RecDesc", desc)
                                                        , ("Sections", intercalate "\n" sections')
                                                        ] tmpl

readSection :: FilePath -> IO Section
readSection secBase = do
  let secFile = secBase  <.> "yaml"
  json <- decodeFileEither (secFile) :: IO (Either ParseException Section) -- XXXjn I'm not  happy with adding the yaml ext here
  case json of 
    Right s -> return s
    Left  e -> do
      putStrLn $ prettyPrintParseException e
      return $ Section {
        practice = secFile
       , title = prettyPrintParseException e
       , intro = Nothing
       , purpose = Nothing
       , background = Nothing
       , asIs = defaultSubSection
       , toBe = defaultSubSection
       }


imgDim :: String -> Maybe Int -> Maybe String -> (String, String)
imgDim attr height dim = case height of
                      Just i -> (attr, printf "height=%f\\textheight, center" ((fromIntegral i)/100.0 :: Float))
                      Nothing -> case dim of
                        Just d -> (attr, d)
                        Nothing -> (attr, "center")


maybePair :: String -> Maybe String -> Maybe (String, String)
maybePair name val = if isJust val then Just (name, fromJust val) else Nothing
                                 

mkImage :: (StringTemplate String) -> String -> String -> Image -> String
mkImage tmpl sec_name def_ov img =
  toString $ setManyAttrib  [ ("Overview", fromMaybe def_ov $ desc img)
                            , (imgDim "ImageDim" (imgheight img) (imgdim img))
                            , ("SectionNoDots", noDots $ sec_name)
                            , ("ModelNoDir", takeFileName $ model img)
                            , ("Subtree", fromMaybe "root" $ (subtree img))
                            , ("Viewport", fromMaybe "" $ (viewport img))
                            ] tmpl

mkImageList :: (STGroup String) -> String -> [Image] -> String -> String -> String
mkImageList tgrp sec_name images not_present ov =
  if null images then ""
  else intercalate "\n" $ map (mkImage (fromJust $ getStringTemplate "image" tgrp) sec_name "") images 

mkAsIsImageList :: Options -> (STGroup String) -> String -> [Image] -> String
mkAsIsImageList opts tgrp sec_name images = mkImageList tgrp sec_name images "not_currently_performed" "as_is_overview" 

mkToBeImageList :: Options -> (STGroup String) -> String -> [Image] -> String
mkToBeImageList opts tgrp sec_name images = mkImageList tgrp sec_name images "not_documented" "to_be_overview"

-- Make the section part of a pattern document.
mkSection :: Options -> Pattern -> (STGroup String) -> Section -> String
mkSection opts pat tgrp sec = do
  let defaultTmpl = fromJust $ getStringTemplate "section" tgrp
      tmpl = defaultTmpl -- fromMaybe defaultTmpl $ getStringTemplate (sec_name sec) tgrp
      to_be = head $ images $ toBe sec  -- XXX this is a hack for the stakeholders 
      to_be_model = model to_be
      to_be_subtree = fromMaybe "root" $ subtree to_be
    in toString $ setManyAttrib  (catMaybes [ Just ("Rec", (pat_name pat))
                                            , Just ("Section", (practice sec))
                                            , Just ("SectionNoDots", (noDots $ practice sec))
                                            , Just ("SecTitle", (title sec))
                                            , maybePair "SecIntro" (intro sec)
                                            , maybePair "SecPurpose" (purpose sec)
                                            , maybePair "SecBackground" (background sec) 
                                            , Just ("ToBeModel", to_be_model)
                                            , Just ("ToBeSubTree", to_be_subtree)
                                            , Just ("AsIsOverview", if isJust $ overview $ asIs sec then fromJust $ overview $ asIs sec
                                                                    else if null $ images $ asIs sec then toString $ fromJust $ getStringTemplate "not_currently_performed" tgrp
                                                                    else toString $ fromJust $ getStringTemplate "as_is_overview" tgrp)
                                            , Just ("AsIsImages", (mkAsIsImageList opts tgrp (practice sec) (images $ asIs sec)))
                                            , Just ("ToBeOverview", if isJust $ overview $ toBe sec then fromJust $ overview $ toBe sec
                                                                    else if null $ images $ toBe sec then toString $ fromJust $ getStringTemplate "not_documented" tgrp
                                                                    else toString $ fromJust $ getStringTemplate "to_be_overview" tgrp)
                                            , Just ("ToBeImages", (mkToBeImageList opts tgrp (practice sec) (images $ toBe sec)))
                                            ]) tmpl

  
mkSectionFiles :: Options ->  (STGroup String) -> Section -> IO ()
mkSectionFiles opts tmpl sec = do
  mkAsisModel opts Nothing sec 
  mkTobeModel opts Nothing sec 

mkPatternDep :: String -> [Section] -> String
mkPatternDep patName secs =
  intercalate "\n" $ map (\s -> printf "%s.md: %s.yaml\n" patName (practice s)) secs
  

mkPatternFiles :: Options ->  (STGroup String) -> Pattern -> IO ()
mkPatternFiles opts tmpl pat = do
  sections <- mapM readSection (pat_sections pat)
  mkPattern opts tmpl pat sections -- ${Rec}.md
  if opt_depends opts || opt_puml opts 
  then do
    mkGoalSubtree opts pat      -- gtm-goal-subtree-$Rec$.png
    mkSubpracSubtree opts pat   -- gtm-subprac-subtree-$Rec$.png
    -- These are necessary because mkAsis/TobeModel generates the dependency.
    mapM_ (mkAsisModel opts $ Just $ pat_name pat) sections -- $Rec$-$AsIsModel$.png, from AsIsModel, AsIsSubtree, AsIsActions
    mapM_ (mkTobeModel opts $ Just $ pat_name pat) sections -- $Rec$-$ToBeModel$.png, from ToBeModel, ToBeSubtree, ToBeActions
    putStrLn $ mkPatternDep (pat_name pat) sections
    return ()
  else if opt_numSections opts then putStrLn $ show $ length $ pat_sections pat
       else return ()

data Pattern = Pattern {
      pat_name :: String
    , pat_sections :: [String]
} deriving (Generic, Show);
instance FromJSON Pattern;
instance ToJSON Pattern;

data Image = Image {
    model :: String
  , desc :: Maybe String
  , subtree :: Maybe String
  , actions :: Maybe [String]
  , imgheight :: Maybe Int
  , imgdim :: Maybe String
  , swimlanes :: Maybe [String]
  , viewport :: Maybe String
} deriving (Generic, Show);
instance FromJSON Image;
instance ToJSON Image;

data Section = Section {
    practice :: String
  , title :: String
  , intro :: Maybe String
  , purpose :: Maybe String
  , background :: Maybe String
  , asIs :: SubSection
  , toBe :: SubSection
} deriving (Generic, Show);
instance FromJSON Section;
instance ToJSON Section;

data SubSection = SubSection {
    overview :: Maybe String
  , images :: [Image]
  } deriving (Generic, Show);
instance FromJSON SubSection;
instance ToJSON SubSection;

defaultSubSection = SubSection { overview = Nothing, images = [] }

data Options = Options {
      opt_gtmdir :: String      -- dir with gtm.xml
    , opt_issuesdir :: String   -- dir with issues/SCAL.csv
    , opt_modelsdir :: String   -- dir with pml models
    , opt_safedir :: String     -- dir with safe practices
    , opt_templatedir :: String 
    , opt_depends :: Bool       -- print dependencies for Makefile?
    , opt_puml :: Bool          -- print puml source too?
    , opt_numSections :: Bool   -- print number of sections?
    , opt_scriptwidth :: Int    -- width of action script text
    , opt_scriptwords :: Int    -- number of words to print from action script
    , opt_parseSection :: Bool
    , opt_input :: [String]     -- input file list
} deriving (Data, Typeable, Show)
 
options :: Options
options = Options { 
            opt_gtmdir = "../gtm"             &= typ "dir" &= help "directory containing gtm.xml"                  &= name "gtmdir"
          , opt_safedir = "../SAFe_practices" &= typ "dir" &= help "directory containing extracted SAFe practices" &= name "safedir"
          , opt_modelsdir = "../models"       &= typ "dir" &= help "directory containing pml models"               &= name "issuesdir"
          , opt_issuesdir = "../issues"       &= typ "dir" &= help "directory containing issues"                   &= name "pmldir"
          , opt_templatedir = "./templates"   &= typ "dir" &= help "directory containing .st templates"            &= name "tmpldir"
          , opt_depends = False               &= typ "boolean" &= help "print make dependencies?"                  &= name "depends"
          , opt_puml = False                  &= typ "boolean" &= help "create puml source?"                       &= name "puml"
          , opt_numSections = False           &= typ "boolean" &= help "count sections?"                           &= name "numsec"
          , opt_scriptwidth = 50              &= typ "int" &= help "width of action script text"                   &= name "scriptwidth"
          , opt_scriptwords = 1000            &= typ "int" &= help "number of words to print from action script"   &= name "scriptwords"
          , opt_parseSection = False          &= typ "boolean" &= help "parse input as section rather than pattern?" &= name "section"
          , opt_input = def                   &= typFile &= args
                }
       &= summary "pattern_content (C) John Noll 2016"
       &= program "pattern_content"
 
main :: IO ()
main = do
  opts <- cmdArgs options
  ts <- directoryGroup (opt_templatedir opts) :: IO (STGroup String)
  if opt_parseSection opts then do
    json <- decodeFileEither ((opt_input opts) !! 0) :: IO (Either ParseException Section)
    case json of
     Right s -> mkSectionFiles opts ts s
     Left e -> putStrLn $ "Error:parseSection:" ++ prettyPrintParseException e
    else do
     json <- decodeFileEither ((opt_input opts) !! 0) :: IO (Either ParseException Pattern)
     case json of 
      Right p -> mkPatternFiles opts ts p
      Left  e -> putStrLn $ "Error:parseSection:" ++ prettyPrintParseException e
