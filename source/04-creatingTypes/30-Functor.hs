-- Functor typeclass is basically for things that can be
-- mapped over. For example, Lists. Let's look at it:
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-- It defines a function (fmap) but it doesn't implement it.
-- It receives a function that goes from a to b and a type
-- constructor (f) of a and returns a type constructor of b.
-- For example, map is defined like:
map :: (a -> b) -> [a] -> [b]
-- This is exactly that, we have array a and array b.
-- So, if we see the definition of the array functor:
instance Functor [] where
    fmap = map
-- It is important to note that the Functor does not apply
-- to a concrete type but to a type constructor. It is not
-- [a] but []

-- Types that can act like a 'box' can be functors. You can
-- think of a list as a box that has an infinite amount of
-- little compartments (elements) where one can be full and
-- the rest empty.
-- Maybe also works like this, it is a box which has Nothing
-- or Just something.
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing
-- REMEMBER! It is Functor Maybe, not Functor (Maybe m), it
-- receives a Type Constructor!!!

-- We can also do this for a Tree (the one we made previously)
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

-- We can do things a little bit weirder. We need a Type
-- Constructor that receives one parameter and returns a
-- type, right? Our type constructor Either needs two parameters
-- so in theory we can't use this. But what happens if we partially
-- apply Either and THEN use it to make a functor?
instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x
-- We only map Right because if we wanted to use the same f
-- to be applied both to Right and Left, then both of them should
-- be of the same type (and we don't care about that, Left is just
-- an error message, we want Right)
