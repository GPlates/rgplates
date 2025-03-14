# Unit tests to see what can be reconstructed - 2025-03-14
# The purpose is to check and update the gws object so checking is turned off.

# if something changes, these will flag them!
library(rgplates)
options(timeout = 5*60)

################################################################################
# 1. Static Polygons
################################################################################

# go through the models one-by-one - static polygons
expect_silent(MULLER2022_sp <- reconstruct("static_polygons", model="MULLER2022", age=30, check=FALSE))
expect_silent(MERDITH2021_sp <- reconstruct("static_polygons", model="MERDITH2021", age=30, check=FALSE))
expect_silent(MULLER2019_sp <- reconstruct("static_polygons", model="MULLER2019", age=30, check=FALSE))
expect_silent(MULLER2016_sp <- reconstruct("static_polygons", model="MULLER2016", age=30, check=FALSE))
expect_silent(MATTHEWS2016_mantle_ref_sp <- reconstruct("static_polygons", model="MATTHEWS2016_mantle_ref", age=30, check=FALSE))
expect_silent(MATTHEWS2016_pmag_ref_sp <- reconstruct("static_polygons", model="MATTHEWS2016_pmag_ref", age=30, check=FALSE))
expect_silent(SETON2012_sp <- reconstruct("static_polygons", model="SETON2012", age=30, check=FALSE))
expect_silent(GOLONKA_sp <- reconstruct("static_polygons", model="GOLONKA", age=30, check=FALSE))
expect_silent(TorsvikCocks2017_sp <- reconstruct("static_polygons", model="TorsvikCocks2017", age=30, check=FALSE))
expect_silent(cao2024_sp <- reconstruct("static_polygons", model="CAO2024", age=30, check=FALSE))
expect_silent(zahirovic2022_sp <- reconstruct("static_polygons", model="ZAHIROVIC2022", age=30, check=FALSE))

# sitll does not work!
expect_error(suppressWarnings(RODINIA2013_sp <- reconstruct("static_polygons", model="RODINIA2013", age=800, check=FALSE)))
expect_error(suppresssWarnings(PALEOMAP_sp <- reconstruct("static_polygons", model="PALEOMAP", age=30, check=FALSE)))
expect_error(suppressWarnings(alfonso2024_sp <- reconstruct("static_polygons", model="ALFONSO2024", age=50, check=FALSE)))
expect_error(supppressWarnings(clennett2020_sp <- reconstruct("static_polygons", model="CLENNETT2020", age=30, check=FALSE)))


################################################################################
# 2. Coastlines
################################################################################

# go through the models one-by-one - coastlines
expect_silent(MULLER2022_coast <- reconstruct("coastlines", model="MULLER2022", age=30, check=FALSE))
expect_silent(MERDITH2021_coast <- reconstruct("coastlines", model="MERDITH2021", age=30, check=FALSE))
expect_silent(MULLER2019_coast <- reconstruct("coastlines", model="MULLER2019", age=30, check=FALSE))
expect_silent(MULLER2016_coast <- reconstruct("coastlines", model="MULLER2016", age=30, check=FALSE))
expect_silent(MATTHEWS2016_mantle_ref_coast <- reconstruct("coastlines", model="MATTHEWS2016_mantle_ref", age=30, check=FALSE))
expect_silent(MATTHEWS2016_pmag_ref_coast <- reconstruct("coastlines", model="MATTHEWS2016_pmag_ref", age=30, check=FALSE))
expect_silent(SETON2012_coast <- reconstruct("coastlines", model="SETON2012", age=30, check=FALSE))
expect_silent(GOLONKA_coast <- reconstruct("coastlines", model="GOLONKA", age=30, check=FALSE))
expect_silent(PALEOMAP_coast <- reconstruct("coastlines", model="PALEOMAP", age=30, check=FALSE))
expect_silent(TorsvikCocks2017_coast <- reconstruct("coastlines", model="TorsvikCocks2017", age=30, check=FALSE))
expect_silent(cao2024_coast <- reconstruct("coastlines", model="CAO2024", age=30, check=FALSE))
expect_silent(zahirovic2022_coast <- reconstruct("coastlines", model="ZAHIROVIC2022", age=30, check=FALSE))
expect_silent(alfonso2024_coastlines <- reconstruct("coastlines", model="ALFONSO2024", age=50, check=FALSE))
expect_silent(clennett2020_coastlines <- reconstruct("coastlines", model="CLENNETT2020", age=30, check=FALSE))

# what does not work
expect_error(suppressWarnings(RODINIA2013_coast <- reconstruct("coastlines", model="RODINIA2013", age=600, check=FALSE)))

################################################################################
# 3. Topological plate Polygons
################################################################################

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
#expect_true(nrow(MATTHEWS2016_pmag_ref_topo)>0)
expect_equal(nrow(MATTHEWS2016_pmag_ref_topo),0)

expect_silent(SETON2012_topo <- reconstruct("plate_polygons", model="SETON2012", age=30, check=FALSE))
expect_true(nrow(SETON2012_topo)>0)

expect_silent(CAO2024_topo <- reconstruct("plate_polygons", model="CAO2024", age=30, check=FALSE))
expect_true(nrow(CAO2024_topo)>0)

expect_silent(ALFONSO2024_topo <- reconstruct("plate_polygons", model="ALFONSO2024", age=30, check=FALSE))
expect_true(nrow(ALFONSO2024_topo)>0)

expect_silent(ZAHIROVIC2022_topo <- reconstruct("plate_polygons", model="ZAHIROVIC2022", age=30, check=FALSE))
expect_true(nrow(ZAHIROVIC2022_topo)>0)

expect_silent(CLENNETT2020_topo <- reconstruct("plate_polygons", model="CLENNETT2020", age=30, check=FALSE))
expect_true(nrow(CLENNETT2020_topo)>0)

# should not be anything!
expect_error(suppressWarnings(RODINIA2013_topo <- reconstruct("plate_polygons", model="RODINIA2013", age=600, check=FALSE)))

expect_silent(GOLONKA_topo <- reconstruct("plate_polygons", model="GOLONKA", age=30, check=FALSE))
expect_equal(nrow(GOLONKA_topo),0)

expect_silent(PALEOMAP_topo <- reconstruct("plate_polygons", model="PALEOMAP", age=30, check=FALSE))
expect_equal(nrow(PALEOMAP_topo),0)

expect_silent(TorsvikCocks2017_topo <- reconstruct("plate_polygons", model="TorsvikCocks2017", age=30, check=FALSE))
expect_equal(nrow(TorsvikCocks2017_topo),0)

################################################################################
# 4. Subduction zones
################################################################################

# go through the models one-by-one - plate topologies - empty!
expect_silent(MULLER2022_sub <- reconstruct("subduction_zones", model="MULLER2022", age=30, check=FALSE))
expect_true(nrow(MULLER2022_sub)>0)

expect_silent(MERDITH2021_sub <- reconstruct("subduction_zones", model="MERDITH2021", age=30, check=FALSE))
expect_true(nrow(MERDITH2021_sub)>0)

expect_silent(MULLER2019_sub <- reconstruct("subduction_zones", model="MULLER2019", age=30, check=FALSE))
expect_true(nrow(MULLER2019_sub)>0)

expect_silent(MULLER2016_sub <- reconstruct("subduction_zones", model="MULLER2016", age=30, check=FALSE))
expect_true(nrow(MULLER2016_sub)>0)

expect_silent(MATTHEWS2016_mantle_ref_sub <- reconstruct("subduction_zones", model="MATTHEWS2016_mantle_ref", age=30, check=FALSE))
expect_true(nrow(MATTHEWS2016_mantle_ref_sub)>0)

expect_silent(MATTHEWS2016_pmag_ref_sub <- reconstruct("subduction_zones", model="MATTHEWS2016_pmag_ref", age=30, check=FALSE))
#expect_true(nrow(MATTHEWS2016_pmag_ref_sub)>0)
expect_equal(nrow(MATTHEWS2016_pmag_ref_sub),0)

expect_silent(SETON2012_sub <- reconstruct("subduction_zones", model="SETON2012", age=30, check=FALSE))
expect_true(nrow(SETON2012_sub)>0)

expect_silent(CAO2024_sub <- reconstruct("subduction_zones", model="CAO2024", age=30, check=FALSE))
expect_true(nrow(CAO2024_sub)>0)

expect_silent(ALFONSO2024_sub <- reconstruct("subduction_zones", model="ALFONSO2024", age=30, check=FALSE))
expect_true(nrow(ALFONSO2024_sub)>0)

expect_silent(ZAHIROVIC2022_sub <- reconstruct("subduction_zones", model="ZAHIROVIC2022", age=30, check=FALSE))
expect_true(nrow(ZAHIROVIC2022_sub)>0)

expect_silent(CLENNETT2020_sub <- reconstruct("subduction_zones", model="CLENNETT2020", age=30, check=FALSE))
expect_true(nrow(CLENNETT2020_sub)>0)

# should not be anything!
expect_error(suppressWarnings(RODINIA2013_sub <- reconstruct("subduction_zones", model="RODINIA2013", age=600, check=FALSE)))

expect_silent(GOLONKA_sub <- reconstruct("subduction_zones", model="GOLONKA", age=30, check=FALSE))
expect_equal(nrow(GOLONKA_sub),0)

expect_silent(PALEOMAP_sub <- reconstruct("subduction_zones", model="PALEOMAP", age=30, check=FALSE))
expect_equal(nrow(PALEOMAP_sub),0)

expect_silent(TorsvikCocks2017_sub <- reconstruct("subduction_zones", model="TorsvikCocks2017", age=30, check=FALSE))
expect_equal(nrow(TorsvikCocks2017_sub),0)

################################################################################
# 5. Plate boundaries
################################################################################

# go through the models one-by-one - plate topologies - empty!
expect_silent(MULLER2022_pb <- reconstruct("plate_boundaries", model="MULLER2022", age=30, check=FALSE))
expect_true(nrow(MULLER2022_pb)>0)

expect_silent(MERDITH2021_pb <- reconstruct("plate_boundaries", model="MERDITH2021", age=30, check=FALSE))
expect_true(nrow(MERDITH2021_pb)>0)

expect_silent(MULLER2019_pb <- reconstruct("plate_boundaries", model="MULLER2019", age=30, check=FALSE))
expect_true(nrow(MULLER2019_pb)>0)

expect_silent(MULLER2016_pb <- reconstruct("plate_boundaries", model="MULLER2016", age=30, check=FALSE))
expect_true(nrow(MULLER2016_pb)>0)

expect_silent(MATTHEWS2016_mantle_ref_pb <- reconstruct("plate_boundaries", model="MATTHEWS2016_mantle_ref", age=30, check=FALSE))
expect_true(nrow(MATTHEWS2016_mantle_ref_pb)>0)

expect_silent(MATTHEWS2016_pmag_ref_pb <- reconstruct("plate_boundaries", model="MATTHEWS2016_pmag_ref", age=30, check=FALSE))
#expect_true(nrow(MATTHEWS2016_pmag_ref_pb)>0)
expect_equal(nrow(MATTHEWS2016_pmag_ref_pb),0)

expect_silent(SETON2012_pb <- reconstruct("plate_boundaries", model="SETON2012", age=30, check=FALSE))
expect_true(nrow(SETON2012_pb)>0)

expect_silent(CAO2024_pb <- reconstruct("plate_boundaries", model="CAO2024", age=30, check=FALSE))
expect_true(nrow(CAO2024_pb)>0)

expect_silent(ALFONSO2024_pb <- reconstruct("plate_boundaries", model="ALFONSO2024", age=30, check=FALSE))
expect_true(nrow(ALFONSO2024_pb)>0)

expect_silent(ZAHIROVIC2022_pb <- reconstruct("plate_boundaries", model="ZAHIROVIC2022", age=30, check=FALSE))
expect_true(nrow(ZAHIROVIC2022_pb)>0)

expect_silent(CLENNETT2020_pb <- reconstruct("plate_boundaries", model="CLENNETT2020", age=30, check=FALSE))
expect_true(nrow(CLENNETT2020_pb)>0)

# should not be anything!
expect_error(suppressWarnings(RODINIA2013_pb <- reconstruct("plate_boundaries", model="RODINIA2013", age=600, check=FALSE)))

expect_silent(GOLONKA_pb <- reconstruct("plate_boundaries", model="GOLONKA", age=30, check=FALSE))
expect_equal(nrow(GOLONKA_pb),0)

expect_silent(PALEOMAP_pb <- reconstruct("plate_boundaries", model="PALEOMAP", age=30, check=FALSE))
expect_equal(nrow(PALEOMAP_pb),0)

expect_silent(TorsvikCocks2017_pb <- reconstruct("plate_boundaries", model="TorsvikCocks2017", age=30, check=FALSE))
expect_equal(nrow(TorsvikCocks2017_pb),0)
