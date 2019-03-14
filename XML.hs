{- Use simple Text.XML library to print a list of children of specified element. -}
module XML where
import Data.List (intercalate)
import Text.Printf (printf)
import Text.XML.Light
import Text.XML.HXT.Core hiding (QName)
import Text.XML.HXT.XPath.Arrows 

import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)

getXpath :: FilePath -> String -> IO (String)
getXpath f p = do
  s <- runX $ readDocument [] f
               >>> getXPathTrees p
               >>> getChildren
               >>> getText
  return $ intercalate " " s

getGTMRec :: FilePath -> String -> IO (String)
getGTMRec gtm recName = getXpath gtm $  printf "//recommendation[@name='%s']/desc" recName 
  
getGTMGrandParentName :: FilePath -> String -> IO (String)
getGTMGrandParentName gtm recName = getXpath gtm $ printf "//sub-practice[@name='%s']/../../@name" recName 

getGTMParentName :: FilePath -> String -> IO (String)
getGTMParentName gtm recName = getXpath gtm $ printf "//sub-practice[@name='%s']/../@name" recName 
