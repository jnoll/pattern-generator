module Utils where 
import Data.Char (isUpper, isSpace, toUpper)

import Data.List (reverse, elem, dropWhileEnd, dropWhile)
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Printf (printf)

data IOType = Input | Output 
  deriving Eq

toListItem :: String -> [Block]
toListItem s = [(Plain [(Str s)])]

mkOrderedList :: [String] -> Block
mkOrderedList bs =
    (OrderedList (1, DefaultStyle, DefaultDelim) $ map toListItem bs)

mkOrderedList' :: [Block] -> Block
mkOrderedList' bs =
    (OrderedList (1, DefaultStyle, DefaultDelim)  [bs])

subst :: String -> String -> [String] -> [String]
subst from to ss =  map (\s -> unwords $ map (\w -> if w == from then to else w) $ words s) ss

wordify :: String -> String
wordify s = map toSpace s

isBracket :: Char -> Bool
isBracket '[' = True
isBracket ']' = True
isBracket _ = False

fromDot :: Char -> Char
fromDot '.' = '_'
fromDot ':' = '_'
fromDot ' ' = '_'
fromDot '/' = '_'
fromDot '-' = '_'
fromDot c = c

fromDotOnly :: Char -> Char
fromDotOnly '.' = '_'
fromDotOnly c = c

noDots :: String -> String
noDots s = map fromDotOnly s

noBrackets :: String -> String
noBrackets = filter (not . isBracket) 

toID :: String -> String
toID s = map fromDot $ filter (not . isBracket) s

toSpace '.' = ' '
toSpace '_' = ' '
toSpace c = c

dotify :: String -> String
dotify s = map toDot s 

toDot :: Char -> Char
toDot '_' = '.'
toDot c = c

stringify :: String -> String
stringify s = printf "\"%s\"" s

noCommas :: String -> String
noCommas s = filter notComma s

notComma :: Char -> Bool
notComma c = c /= ','

fromUS :: Char -> Char
fromUS '_' = ' '
fromUS c = c


fromSpace :: Char -> Char
fromSpace ' ' = '_'
fromSpace c = c

stripSpace :: String -> String
stripSpace s = dropWhileEnd isSpace $ dropWhile isSpace s

allUpper :: String -> Bool
allUpper s = all  (\c -> isUpper c || isSpace c) s

capitalize :: String -> String
capitalize [] = []
capitalize (h:r) = (toUpper h):r

-- Turn "CamelCase" into "Camel Case"
decamelize :: String -> String
decamelize s = stripSpace $  if allUpper s then s else decamelize' s []

decamelize' :: String -> String -> String
decamelize' [] r = reverse r
decamelize' (c1:[]) r = decamelize' [] (c1:r)
--decamelize' (c1:s) [] = decamelize' s [c1]
decamelize' (c1:s) r = if (isUpper c1) && (not $ isUpper $ head s)  then decamelize' s (c1:' ':r) else decamelize' s (c1:r)

-- Turn a Resource specification into a natural language phrase.
-- Ex: Foo.exists becomes "Foo exists"
-- XXXjn probably this should be done at the SPEC level so predicates can be translated into English:
-- Foo.exists == "true" s.b. "Foo exists", Bar.timestamp > "today" s.b. "Bar's timestamp is greater than today"
-- Unfortunately this is hard to do in general: Bar.modified s.b. "Bar is modified" rather than "Bar modified" 
-- or "Bar's modified is true".
naturalize :: IOType ->  String -> String
naturalize _ s = map (fromUS . fromDot) s

