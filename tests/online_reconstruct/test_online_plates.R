library(rgplates)
options(timeout = 5*60)

# Pure plates

################################################################################
# today
expect_silent(
	rec0 <- reconstruct("static_polygons", 0, model="MERDITH2021")
)

# correct type
expect_inherits(rec0, "sf")

# correct crs
expect_equal(sf::st_crs(rec0)[[1]], "4326")


################################################################################
# past
expect_silent(
	rec140 <- reconstruct("static_polygons", 140, model="MERDITH2021")
)

expect_inherits(rec140, "sf")
expect_equal(sf::st_crs(rec140)[[1]], "4326")

################################################################################
# Multiple
expect_silent(
	rec <- reconstruct("static_polygons", age=c(0, 140), model="MERDITH2021")
)

expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "140"))
expect_equal(rec[["0"]], rec0)
expect_equal(rec[["140"]], rec140)

################################################################################
# Test inaccurate ages! - 
# Reconstructed to 140.4 
expect_silent(
	rec140_4 <- reconstruct("static_polygons", age=140.4, model="MERDITH2021")
)	

expect_inherits(rec140_4, "sf")
expect_equal(sf::st_crs(rec140_4)[[1]], "4326")

################################################################################
# Multiple models
# Seton model
expect_silent(
	rec <- reconstruct("plate_polygons", 0, model="SETON2012")
)
expect_inherits(rec, "sf")
expect_equal(sf::st_crs(rec)[[1]], "4326")

# Müller 2016
expect_silent(
	rec <- reconstruct("plate_polygons", 0, model="MULLER2016")
)
expect_inherits(rec, "sf")
expect_equal(sf::st_crs(rec)[[1]], "4326")

# the GOLONKA model
expect_silent(
	rec <- reconstruct("static_polygons", 0, model="GOLONKA")
)
## expect_inherits(rec, "sf")
## expect_equal(sf::st_crs(rec)[[1]], "4326")

# the scotese model
expect_error(
	rec <- reconstruct("static_polygons", 0, model="PALEOMAP")
)
## expect_inherits(rec, "sf")
## expect_equal(sf::st_crs(rec)[[1]], "4326")

# Matthew 2016
expect_silent(
	rec <- reconstruct("plate_polygons", 0, model="MATTHEWS2016_pmag_ref")
)
expect_inherits(rec, "sf")
expect_equal(sf::st_crs(rec)[[1]], "4326")

# invalid input
suppressWarnings(
	expect_error(
		rec <- reconstruct("static_polygons", 0, model="GARBAGE")
	)
)
