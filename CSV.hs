{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
--import Control.Applicative ((<$>), (<*>))
module CSV where
import Utils 
import Data.List (map)
import Text.CSV.Lazy.String
import Text.Pandoc.Shared (trim) -- overkill, but Pandoc is included anyway

select :: Int -> String -> [String] -> Bool
select f key row = key == (trim $ row !! f)

project :: Int -> [String] -> String
project f row = row !! f

getCSV :: Char -> String -> [[String]]
getCSV delim contents = let (head:rows) = fromCSVTable $ csvTableFull $ parseDSV False delim contents
                            in rows

lookupCSV :: String -> Int -> Int -> [[String]] -> [String]
lookupCSV key kfield pfield rows = map (project pfield) $ filter (select kfield key) rows


