-- Modules are imported very easily:

-- This imports all functions that are exported in Data.List
import Data.List

-- Here we only bring those two functions
import Data.List (nub, sort)

-- Here we bring all of them except for nub
import Data.List hiding (nub)

-- Here we say that functions from this module need to be called with
-- Data.Map.filter
import qualified Data.Map

-- Or with M.filter
import qualified Data.Map as M
