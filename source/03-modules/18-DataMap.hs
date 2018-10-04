-- Dictionaries or Maps are also called Association Lists in Haskell.
import qualified Data.Map as Map

-- A way to represent association lists is by having lists of pairs
phoneBook =
  [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
  ]

-- Function to find a key in a map
getByKey :: (Eq k) => k -> [(k, v)] -> v
getByKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs
-- To avoid getting an error when the key is not in the map, we can return a
-- Maybe.
getByKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
getByKey' _ [] = Nothing
getByKey' key ((k,v):xs) = if key == k
                           then Just v
                           else getByKey' key xs
-- We can do that with folding too.
getByKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
getByKey'' key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing

-- The Data.Map implementation of Map is better than the list of pairs, and we
-- create one the following way:
createMap associationList = Map.fromList associationList
-- The only problem with these maps is that the constraint for the keys passes
-- from Eq to Ord!!!!

-- Create an empty map
emptyMap = Map.empty
-- Adding pairs to the map
insertElements = Map.insert "key" "value" emptyMap
-- This can be queued
insertElements' = Map.insert 1 100 . Map.insert 2 200 . Map.insert 3 300 $ emptyMap

-- Check for emptiness
isEmpty _map = Map.null _map

-- Map with one element
singletonMap = Map.singleton "key" "value"

-- lookup. Is a get for maps; returns a Maybe
exampleLookup = Map.lookup 1 $ Map.singleton 1 2

-- member. This is like the elem of lists

-- map and filter are the same as the normal ones but they apply to the values
-- of the dictionary

-- keys === map fst . Map.toList
-- elems === map snd . Map.toList

-- fromListWith. This is like a fromList but instead of removing duplicate keys
-- it receives a function to decide what to do with them
phoneBook' =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]
-- Example
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

-- instertWith. This follows the same logic as the previous one; if we try to
-- insert something that is already present, then it uses the function to define
-- what to do.
