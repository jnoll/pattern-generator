{-# LANGUAGE QuasiQuotes #-} 
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveDataTypeable #-} 

module Main where
import Control.Monad.Reader (Reader(..), ask, runReader)
import Data.Char (toLower)
import Data.List (intercalate, filter)
import Data.Text (Text(..), pack, unpack)
import Data.Maybe (fromJust)
import NLP.Tokenize.String (tokenize)
import NLP.Types (Corpus(..), addDocument, mkCorpus)
import NLP.Similarity.VectorSim (similarity)
import NLP.Stemmer (Stemmer(..), stem)
import NLP.RAKE.Text (stopword, pSplitter, defaultStoplist, defaultNolist)
import System.Console.CmdArgs 
import System.Environment (getArgs, getProgName)
import Text.CSV.Lazy.String
import Text.EditDistance (defaultEditCosts, levenshteinDistance, restrictedDamerauLevenshteinDistance)
import Text.Fuzzy (Fuzzy(..), match, test)

downcase :: String -> String
downcase s = map toLower s

getCSV :: Char -> String -> ([String], [[String]])
getCSV delim contents = let (head:rows) = fromCSVTable $ csvTableFull $ parseDSV False delim contents in
                        (head, rows)

untokenize :: [String] -> String
untokenize l = intercalate " " l

stopW :: String -> Bool
stopW s = not $ stopword defaultStoplist [".", ",", "-", ":", "(", ")", "/)", "[", "]", " "] $ pack s


canon :: String -> [Text]
canon s = map pack $ filter stopW $ map ((stem English) . downcase) $ tokenize s 

canon' :: String -> String
canon' s = intercalate " " $ map unpack $ canon s

makeCorpus :: [[String]] -> [[String]] -> (Reader Options) Corpus
makeCorpus srcs tgts = ask >>= \opts ->
    let src_docs = map (\l -> if length l > (opt_src opts) then canon $ l !! (opt_src opts) else [] ) srcs 
        tgt_docs = map (\l -> if length l > (opt_target opts) then canon $ l !! (opt_target opts) else [] ) tgts
    in return $ mkCorpus (src_docs ++ tgt_docs) 

compareRaw :: Bool -> String -> String -> Bool
compareRaw canon src tgt = 
    if canon then canon' src == canon' tgt else src == tgt
compareCosine :: Corpus -> String -> (Int, [String]) -> [[String]] -> (Reader Options) (Int, [String])
compareCosine  _ _ (score, best) [] = return $ (score, best)
compareCosine  c src (score, best) (s:ss) = ask >>= (\opts ->
    if length s > (opt_target opts) then
        let target = s !! (opt_target opts)
            corpus = c 
        in if compareRaw (opt_canon opts) src target then return $ (101, s) 
           else let score' =  round $ (*) 100.0 $ similarity corpus (canon src) (canon target)
                in if score' > score then compareCosine c src (score', s) ss else compareCosine c src (score, best) ss
    else compareCosine c src (score, best) ss )

compareLev :: Corpus -> String -> (Int, [String]) -> [[String]] -> (Reader Options) (Int, [String])
compareLev _ _ (score, best) [] = return $ (score, best)
compareLev c src (score, best) (s:ss) = ask >>= (\opts ->
    if length s > (opt_target opts) then
        let target = if (opt_canon opts) then canon' $ s !! (opt_target opts) else  s !! (opt_target opts)
        in if compareRaw (opt_canon opts) src target then return $ (0, s) 
           else let score' = levenshteinDistance defaultEditCosts src target
                in if score' < score then compareLev c src (score', s) ss else compareLev c src (score, best) ss
    else compareLev c src (score, best) ss )

-- Doesn't work well on the SAFe data.
compareFuzzy :: Corpus ->  String -> (Int, [String]) -> [[String]] -> (Reader Options) (Int, [String])
compareFuzzy  _ _ (score, best) [] = return $ (score, best)
compareFuzzy c src (score, best) (s:ss) = ask >>= (\opts ->
    if length s > (opt_target opts) then
        let target = if (opt_canon opts) then canon' $ s !! (opt_target opts) else  s !! (opt_target opts)
            threshold = 0
        in if compareRaw (opt_canon opts) src target then return (909909, s) else -- arbitrary easily recognizable high number
               case match src target "" "" id False of
                 Just (Fuzzy _ _ score') -> if score' > score then compareFuzzy c src (score', s) ss 
                                            else compareFuzzy c src (score, best) ss
                 otherwise -> compareFuzzy c src (score, best) ss
    else compareFuzzy c src (score, best) ss )


compareTo :: Corpus ->  [[String]] -> [String] -> (Reader Options) [String]
compareTo corpus ss a = ask >>= (\opts ->
    if length a > 2 then 
        let src = a !! (opt_src opts)
        in case opt_type opts of 
                              CompareLev    -> compareLev corpus  src (10000, ["initial"]) ss 
                              CompareCosine -> compareCosine corpus  src (0, ["initial"]) ss 
                              CompareFuzzy  -> compareFuzzy corpus  src (0, ["initial"]) ss 
                              otherwise     -> compareLev corpus src (10000, ["initial"]) ss 
           >>= (\(score, best) -> return $ (show score):(a ++ best) )
    else return a )

join :: [[String]] -> [[String]] -> (Reader Options) [[String]]
join  rs1 rs2 = makeCorpus rs1 rs2 >>= (\corpus -> mapM (compareTo corpus rs2) rs1)
      
-- help, summary, and program are for command line argument parsing.  
data ComparisonType = CompareStrict | CompareFuzzy | CompareCosine | CompareLev
  deriving (Data, Typeable, Show)
data Options = Options {
      opt_canon :: Bool
    , opt_type :: ComparisonType
    , opt_src :: Int
    , opt_target :: Int
    , opt_delim :: String
    , opt_files :: [String]
} deriving (Data, Typeable, Show)

defaultOptions :: Options
defaultOptions = Options { 
                   opt_canon = False &= help "Canonicalize strings (downcase, stem, remove stopwords)?" &= explicit &= name "canon"
                 , opt_type = enum [ CompareCosine &= help "Use cosine document comparison"
                                   , CompareFuzzy  &= help "Use fuzzy string comparison"
                                   , CompareLev    &= help "Use Levenshtein edit distance to compare strings"
                                   , CompareStrict &= help "Use strict string comparison" 
                                   ]
                 , opt_src = 1     &= help "Source field to compare (file 1)" &= name "src"
                 , opt_target = 1    &= help "Destination field to compare (file 2)" &= name "target"
                 , opt_delim = "|" &= help "Field delimter (default '|')"
                 , opt_files = def &= args
               }
       &= summary "ajoin v0.1, (C) 2016 John Noll"
       &= program "compare_dsv"



main :: IO ()
main = do
  opts <- cmdArgs defaultOptions
  if length (opt_files opts) < 2 then 
      getProgName >>= (\p ->  putStrLn $ "usage: " ++ p ++ "[options] file1 file2")
  else  do
      c1 <- readFile ((opt_files opts) !! 0)
      c2 <- readFile ((opt_files opts) !! 1)
      let (_, rs) = getCSV (head  $ opt_delim opts) c1
      let (_, rs') = getCSV (head $ opt_delim opts) c2
      putStrLn $ intercalate "\n" $ map (intercalate " | ") $ runReader (join rs rs') opts
