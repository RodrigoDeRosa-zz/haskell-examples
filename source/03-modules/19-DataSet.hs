-- Sets are mathematical Sets. Each element appears only one and they must
-- implement Ord
import qualified Data.Set as Set

text = "The old man left his garbage can out and now his trash is all over my lawn!"
setFromList = Set.fromList text
-- => fromList " !Tabcdefghilmnorstuvwy"

-- Here, we have all the set functions we'd expect.

-- intersection
-- difference
-- union
-- null
-- size
-- member
-- empty
-- singleton
-- insert
-- delete
-- isSubsetOf
-- map
-- filter
