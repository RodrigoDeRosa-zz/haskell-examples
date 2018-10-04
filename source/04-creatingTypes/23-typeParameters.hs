-- Type constructors can take types as parameters to produce new types.
-- For example, Maybe:

--    data Maybe a = Nothing | Just a

-- Here, a is also a type; what we get is a new type. For example Just Int or
-- Just [Char].
-- Maybe is not a type, there can be nothing created of type Maybe because it
-- is just a Type Constructor

-- Here, we make a 3D vector of the type we want
data Vector a = Vector a a a deriving (Show)
-- We can define a function for Num Vectors
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)
-- Example
exampleVplus = Vector 3 5 8 `vplus` Vector 9 2 8
