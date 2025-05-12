library(rgplates)

# basic argumentation
expect_silent(sfEdge <- mapedge())
expect_true(inherits(sfEdge, "sfc"))
expect_equal(st_crs(sfEdge), st_crs("EPSG:4326"))

# automatic projection change
expect_silent(sfMoll <- mapedge(crs="ESRI:54009"))
expect_true(inherits(sfMoll, "sfc"))
expect_equal(st_crs(sfMoll), st_crs("ESRI:54009"))

# with sp
library(sp)
expect_silent(spEdge <- mapedge(out="sp"))
expect_true(inherits(spEdge, "Spatial"))


