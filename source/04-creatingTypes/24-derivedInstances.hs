-- Haskell can automatically make our types an instance of any of the following:
-- Eq, Ord, Enum, Bounded, Show or Read; it can derive the behaviour of our types
-- in these contexts if we use the deriving keyword

data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int
} deriving (Eq, Show, Read) -- It can derive as many as you want

dude1 = Person {firstName = "John", lastName = "Doe", age = 22}
dude2 = Person {firstName = "Tom", lastName = "Keat", age = 33}

exampleNotEq = dude1 == dude2
exampleEq = dude1 == Person {firstName = "John", lastName = "Doe", age = 22}

-- Read allows to build a data Type from string:
dude3 = read "Person {firstName=\"Michael\",lastName=\"Diamond\",age=43}" :: Person
exampleCompareRead = read "Person {firstName=\"John\",lastName=\"Doe\",age=22}" == dude1

-- We can use Enum and Bounded to create enumerations:
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- An Ord data type respects the order in which we defined things. This allows
-- us to use succ, pred, [Monday .. Thursday], >, <, compare, etc.
