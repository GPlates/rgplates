library(rgplates)

# the used objects
model <- chronosphere::fetch("paleomap", "model", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)

################################################################################
# today
rec0 <- reconstruct("plates", 0, model=model)


# correct type
expect_inherits(rec0, "sf")

# correct crs
expect_equal(sf::st_crs(rec0)[[1]], "WGS 84")


################################################################################
# past
rec140 <- reconstruct("plates", 140, model=model)


expect_inherits(rec140, "sf")
expect_equal(sf::st_crs(rec140)[[1]], "WGS 84")

################################################################################
# Multiple
rec <- reconstruct("plates", age=c(0, 140), model=model)


expect_inherits(rec, "list")
expect_equal(names(rec), c("0", "140"))
expect_equal(rec[["0"]], rec0)
expect_equal(rec[["140"]], rec140)

