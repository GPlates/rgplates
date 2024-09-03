# Unit tests to see what can be reconstructed - 2024-09-03
# The purpose is to check and update the gws object so checking is turned off.

# if something changes, these will flag them!
library(rgplates)

# go through the models one-by-one - static polygons
expect_silent(MULLER2022_sp <- reconstruct("static_polygons", model="MULLER2022", age=30, check=FALSE))
expect_silent(MERDITH2021_sp <- reconstruct("static_polygons", model="MERDITH2021", age=30, check=FALSE))
expect_silent(MULLER2019_sp <- reconstruct("static_polygons", model="MULLER2019", age=30, check=FALSE))
expect_silent(MULLER2016_sp <- reconstruct("static_polygons", model="MULLER2016", age=30, check=FALSE))
expect_silent(MATTHEWS2016_mantle_ref_sp <- reconstruct("static_polygons", model="MATTHEWS2016_mantle_ref", age=30, check=FALSE))
expect_silent(MATTHEWS2016_pmag_ref_sp <- reconstruct("static_polygons", model="MATTHEWS2016_pmag_ref", age=30, check=FALSE))
expect_error(suppressWarnings(RODINIA2013_sp <- reconstruct("static_polygons", model="RODINIA2013", age=600, check=FALSE)))
expect_silent(SETON2012_sp <- reconstruct("static_polygons", model="SETON2012", age=30, check=FALSE))
expect_silent(GOLONKA_sp <- reconstruct("static_polygons", model="GOLONKA", age=30, check=FALSE))
expect_error(suppresssWarnings(PALEOMAP_sp <- reconstruct("static_polygons", model="PALEOMAP", age=30, check=FALSE)))
expect_silent(TorsvikCocks2017_sp <- reconstruct("static_polygons", model="TorsvikCocks2017", age=30, check=FALSE))

# All but Rodinia and Paleomap

# go through the models one-by-one - coastlines
expect_silent(MULLER2022_coast <- reconstruct("coastlines", model="MULLER2022", age=30, check=FALSE))
expect_silent(MERDITH2021_coast <- reconstruct("coastlines", model="MERDITH2021", age=30, check=FALSE))
expect_silent(MULLER2019_coast <- reconstruct("coastlines", model="MULLER2019", age=30, check=FALSE))
expect_silent(MULLER2016_coast <- reconstruct("coastlines", model="MULLER2016", age=30, check=FALSE))
expect_silent(MATTHEWS2016_mantle_ref_coast <- reconstruct("coastlines", model="MATTHEWS2016_mantle_ref", age=30, check=FALSE))
expect_silent(MATTHEWS2016_pmag_ref_coast <- reconstruct("coastlines", model="MATTHEWS2016_pmag_ref", age=30, check=FALSE))
expect_error(suppressWarnings(RODINIA2013_coast <- reconstruct("coastlines", model="RODINIA2013", age=600, check=FALSE)))
expect_silent(SETON2012_coast <- reconstruct("coastlines", model="SETON2012", age=30, check=FALSE))
expect_silent(GOLONKA_coast <- reconstruct("coastlines", model="GOLONKA", age=30, check=FALSE))
expect_silent(PALEOMAP_coast <- reconstruct("coastlines", model="PALEOMAP", age=30, check=FALSE))
expect_silent(TorsvikCocks2017_coast <- reconstruct("coastlines", model="TorsvikCocks2017", age=30, check=FALSE))

# All but Rodinia

# go through the models one-by-one - plate topologies - empty!
expect_silent(MULLER2022_topo <- reconstruct("plate_polygons", model="MULLER2022", age=30, check=FALSE))
expect_true(nrow(MULLER2022_topo)>0)

expect_silent(MERDITH2021_topo <- reconstruct("plate_polygons", model="MERDITH2021", age=30, check=FALSE))
expect_true(nrow(MERDITH2021_topo)>0)

expect_silent(MULLER2019_topo <- reconstruct("plate_polygons", model="MULLER2019", age=30, check=FALSE))
expect_true(nrow(MULLER2019_topo)>0)

expect_silent(MULLER2016_topo <- reconstruct("plate_polygons", model="MULLER2016", age=30, check=FALSE))
expect_true(nrow(MULLER2016_topo)>0)

expect_silent(MATTHEWS2016_mantle_ref_topo <- reconstruct("plate_polygons", model="MATTHEWS2016_mantle_ref", age=30, check=FALSE))
expect_true(nrow(MATTHEWS2016_mantle_ref_topo)>0)

expect_silent(MATTHEWS2016_pmag_ref_topo <- reconstruct("plate_polygons", model="MATTHEWS2016_pmag_ref", age=30, check=FALSE))
expect_true(nrow(MATTHEWS2016_pmag_ref_topo)>0)

expect_error(suppressWarnings(RODINIA2013_topo <- reconstruct("plate_polygons", model="RODINIA2013", age=600, check=FALSE)))

expect_silent(SETON2012_topo <- reconstruct("plate_polygons", model="SETON2012", age=30, check=FALSE))
expect_true(nrow(SETON2012_topo)>0)

expect_silent(GOLONKA_topo <- reconstruct("plate_polygons", model="GOLONKA", age=30, check=FALSE))
expect_equal(nrow(GOLONKA_topo),0)

expect_silent(PALEOMAP_topo <- reconstruct("plate_polygons", model="PALEOMAP", age=30, check=FALSE))
expect_equal(nrow(PALEOMAP_topo),0)

expect_silent(TorsvikCocks2017_topo <- reconstruct("plate_polygons", model="TorsvikCocks2017", age=30, check=FALSE))
expect_equal(nrow(TorsvikCocks2017_topo),0)
