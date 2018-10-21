-- These are data types in which constructors we have
-- the same data type.
-- The list is an example of this. [5] is the same as 5:[];
-- in the left side there's a number and on the right, a list.
-- Same goes for 3:4:5:6:[], which accumulates list until it
-- finishes.
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- Basically, we have an element of type a, followed by a list that
-- may be empty or may be another element of type a.
-- For example: 1:(2:(3:([])))

-- Fixity declarations. We can define how tightly will operators bind
-- to it and whether it is left or right associative
infixl 6 +
infixl 7 *
-- Here, we see that both * and + are left operators but * binds
-- stronger than +
