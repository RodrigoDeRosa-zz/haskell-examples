-- Let's look at how Eq is defined:
class Eq equatable where
    (==) :: equatable -> equatable -> Bool
    (/=) :: equatable -> equatable -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-- This says that two elements are equal it they are not
-- different and different if they are not equal. They
-- are defined in terms of mutual recursion.
data TrafficLight = Red | Yellow | Green
-- Now we will indicate how it implements the Eq functions
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
-- We only needed to define == because Eq already implements
-- how /= works, related to ==.
-- If we didn't have the functions in terms of mutual recursion,
-- we would have to implement everything for TrafficLight

-- Now we can implement Show
instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- It is also possible to implement a class that is a subclass
-- of another type class. For example:
class (Eq a) => Num a where ....

-- We could also do something like:
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-- We have the constraint because we need to be sure that
-- the type m is also comparable.