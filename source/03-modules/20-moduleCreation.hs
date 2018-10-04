-- We can import the functions that a module exports. For this part, we will
-- create our own module in Geometry.hs and then import it here.

-- We can implement the Geometry module in two ways. The first one is the one
-- in Geometry.hs. The second one is dividing all the parts we identify in
-- different files and creating sub-modules of Geometry
 import Geometry
 import Geometry.Sphere as Sphere
 import Geometry.Cuboid as Cuboid
 import Geometry.Cube as Cube
