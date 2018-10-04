-- data Bool = False | True

-- data means that we're defining a new data type. The part before the = denotes
-- the type, which is Bool. The parts after the = are value constructors. They
-- specify the different values that this type can have. The | is read as or.
-- So we can read this as: the Bool type can have a value of True or False.
-- Both the type name and the value constructors have to be capital cased.

module Shape(
  Point(..),
  Shape(..),
  surface,
  nudge
) where
-- The dots mean we are exporting all value constructors for Shape. That means
-- that whoever imports Shape can use Circle and Rectangle

-- We can define our own data type Shape:
data Shape = Circle Float Float Float
  | Rectangle Float Float Float Float
  deriving (Show) -- This makes it printable!
-- This means that the Circle constructor receives 3 floats (centerX, centerY, radius)
-- and returns a Circle. The Rectangle receives 4 floats (upperLeftX, upperLeftY,
-- bottomRightA, bottomRightB) and returns a Rectangle

--    circleType = :t Circle
-- => Circle :: Float -> Float -> Float -> Shape

--    rectangleType = :t Rectangle
-- => Rectangle :: Float -> Float -> Float -> Float -> Shape

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- Examples of use
exampleSurface = surface $ Circle 2 2 1
-- => pi
exampleSurface' = surface $ Rectangle 0 0 10 10
-- => 100.0

-- Here we are partially applying the constructor for each element of the list
createCircles = map (Circle 10 20) [4,5,6]
-- => [Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0]

-- We can now start creating middle data types
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)
-- Now we can refactor our surface function
surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- We only changed the patterns but not the calculation

-- Moving shapes
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
