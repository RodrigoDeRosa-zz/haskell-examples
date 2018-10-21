-- Let's implement something like a polymorphic class which
-- determines, for each type, what is considered True and what
-- is considered False.
class YesNo a where
    yesno :: a -> Bool
-- Ints
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
-- Lists
instance YesNo [a] where
    yesno [] = False
    yesno _ = True
-- Booleans
instance YesNo Bool where
    yesno = id -- id returns exactly what it received
-- Maybes
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
-- Trees (the ones we implemented in 27)
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True
-- TrafficLights
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
-- The following function mimics an if statement but with YesNo values
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
-- An example of use would be:
-- yesnoIf [] "YEAH!" "NO!" => "NO!"
-- yesnoIf [1] "YEAH!" "NO!" => "YEAH!