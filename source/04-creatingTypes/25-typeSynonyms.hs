-- An example of type synonyms are Strings:
type String = [Char]

-- Also if we had, for example:
phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]
-- The PhoneBook could be defined as
type PhoneBook = [(String, String)]
-- Or, also:
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
-- From this, we could define
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

-- Type synonyms can also be parameterized:
type AssociationList k v = [(k,v)]
-- This is a type constructor that takes two types and produces another
-- concrete type. For example:
AssociationList Int String

-- We can get partially applied type constructors in the same way we do
-- with functions:
type IntMap v = Map Int v
type IntMap = Map Int
-- The two examples above are exactly the same because Map expects 2 types.

-- Either.
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- This data type is used to encapsulate data of one type (a) or another (b)
-- Usually, both Left and Right are pattern matched. This is used for
-- functions that can fail. It is common to store the error on Left and
-- the successful result (if there was any) on Right.

-- An example could be done with Lockers in a school. We have lockers which have a
-- code and a state and a map to store them
import qualified Data.Map as Map
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
-- We can check if a certain locker is taken or not (if it exists) and return
-- it in an Either
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
-- So, if the locker does not exist, we return a Left; if it is already
-- taken, we also return a Left and if it is not taken, then we return
-- a Right with the locker's code. Each of the Lefts have their error
-- message inside them.