-- We will diagram this problem by saying that we have Avenue A, Avenue B and Streets C.
-- Then the data file will have, one per line, wA then wB and then wC.
-- An example of this would be:
--    50
--    30
--    5
--    10
--    25
--    10
-- So, to get to A1 through A would have a weight of 50, and to get through B+C would be 25.
-- The 10 after the 5 represents the weight between A1 and A2, meanwhile 25 represents the
-- weight between B2 and A2 (and the first 5 is the same for A1, B1). The same goes for B.

import Data.List

-- Let's define the model for our problem
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

-- For example, we could do:
heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

-- We create an enumeration of A, B and C
data Label = A | B | C deriving (Show)
-- A path will now tell us each label and it's weight
type Path = [(Label, Int)]
-- For example, if we applied optimalPath to heathrowToLondon, it should return the path:
-- [(B,10), (C,30), (A,5), (C,20), (B,2), (B,8)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  -- The second element of both paths is the weight
  let priceA = sum $ map snd pathA -- sum [] == 0
      priceB = sum $ map snd pathB -- sum [] == 0
      -- We calculate the price of each possible step
      forwardPriceToA = priceA + a
      forwardPriceToB = priceB + b
      crossPriceToA   = priceB + b + c
      crossPriceToB   = priceA + a + c
      -- Now we decide which of them we set for A
      newPathToA = if forwardPriceToA <= crossPriceToA
                      then (A, a):pathA
                      else (C, c):(B, b):pathB
      -- And the same for B
      newPathToB = if forwardPriceToB <= crossPriceToB
                      then (B, b):pathB
                       else (C, c):(A, a):pathA
      -- Finally, we return both paths
      in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
-- For a given roadSystem, we fold it with our previous function
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
  -- Now we check which of the two paths is best
  in if sum (map snd bestAPath) <= sum (map snd bestBPath)
    then reverse bestAPath
    else reverse bestBPath

-- We define a function that takes groups of n elements from a list
-- For example groupsOf 3 [1..10] would return [1,2,3],[4,5,6],[7,8,9],[10]
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- Now let's define our main
main = do
  -- We read the input
  contents <- getContents
  -- Get the input in groups of threes
  let threes = groupsOf 3 (map read $ lines contents)
      -- Convert the input to a roadSystem
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      -- Calculate the optimal path
      path = optimalPath roadSystem
      -- Now we separate the result for printing
      pathString = concat $ map (show . fst) path
      pathPrice = sum $ map snd path
  -- Printing
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice