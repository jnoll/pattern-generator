{-# LANGUAGE DeriveDataTypeable #-}
module Main where
 
import CSV
import PML
import Utils

import Data.List (intercalate)
import System.Console.CmdArgs


nonewlines :: String -> String 
-- from https://stackoverflow.com/questions/19545253/haskell-replace-characters-in-string
nonewlines = map (\c -> if c == '\n' then ' ' else c) 
-- nonewlines [] = []
-- nonewlines ('\n':r) = ' ':nonewlines r
-- nonewlines (c:r) = c:nonewlines r


pml2list :: String ->  String -> [[String]]
pml2list filename pml = map (\(act_nm, _, script, _) -> [filename, act_nm, nonewlines script]) $ getActions pml "root" Nothing
    
data Options = Options {
    opt_input :: [String]     -- input file list
} deriving (Data, Typeable, Show)
 
options :: Options
options = Options { 
  opt_input = def                   &= typFile &= args
  }
       &= summary "pattern_content (C) John Noll 2016"
       &= program "pattern_content"
 
doxfrm :: String -> IO String
doxfrm filename = do
  pml <- readFile filename
  return $ intercalate "\n" $  map (\r -> intercalate "|" r) $ pml2list filename pml

main :: IO ()
main = do
  opts <- cmdArgs options
  rs <- mapM doxfrm $ opt_input opts
  putStrLn $ intercalate "\n" $ rs
