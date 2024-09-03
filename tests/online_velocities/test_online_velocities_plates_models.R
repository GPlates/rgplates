# Unit tests to check that gws entries can be reconstrcuted propoerliy
library(rgplates)

# Explicit tests
#
# all should be returning something at various age-depth!
# 30 Ma
expect_silent(current <- velocities("static_polygons", model="MULLER2022", age=30, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MERDITH2021", age=30, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MULLER2019", age=30, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MULLER2016", age=30, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MATTHEWS2016_mantle_ref", age=30, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MATTHEWS2016_pmag_ref", age=30, output="SpatRaster"))
# expect_silent(current <- velocities("static_polygons", model="RODINIA2013", age=600, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="SETON2012", age=30, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="GOLONKA", age=30, output="SpatRaster"))
expect_error(current <- velocities("static_polygons", model="PALEOMAP", age=30, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="PALEOMAP", age=30, output="SpatRaster", check=FALSE)) # only because not in gws!
expect_silent(current <- velocities("static_polygons", model="TorsvikCocks2017", age=30, output="SpatRaster"))

# 100 Ma
expect_silent(current <- velocities("static_polygons", model="MULLER2022", age=100, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MERDITH2021", age=100, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MULLER2019", age=100, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MULLER2016", age=100, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MATTHEWS2016_mantle_ref", age=100, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MATTHEWS2016_pmag_ref", age=100, output="SpatRaster"))
# expect_silent(current <- velocities("static_polygons", model="RODINIA2013", age=600, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="SETON2012", age=100, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="GOLONKA", age=100, output="SpatRaster"))
expect_error(current <- velocities("static_polygons", model="PALEOMAP", age=100, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="PALEOMAP", age=150, output="SpatRaster", check=FALSE)) # only because not in gws!
expect_silent(current <- velocities("static_polygons", model="TorsvikCocks2017", age=100, output="SpatRaster"))

# 150 Ma
expect_silent(current <- velocities("static_polygons", model="MULLER2022", age=150, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MERDITH2021", age=150, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MULLER2019", age=150, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MULLER2016", age=150, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MATTHEWS2016_mantle_ref", age=150, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="MATTHEWS2016_pmag_ref", age=150, output="SpatRaster"))
# expect_silent(current <- velocities("static_polygons", model="RODINIA2013", age=600, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="SETON2012", age=150, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="GOLONKA", age=150, output="SpatRaster"))
expect_error(current <- velocities("static_polygons", model="PALEOMAP", age=150, output="SpatRaster"))
expect_silent(current <- velocities("static_polygons", model="PALEOMAP", age=150, output="SpatRaster", check=FALSE)) # only because not in gws!
expect_silent(current <- velocities("static_polygons", model="TorsvikCocks2017", age=150, output="SpatRaster"))
