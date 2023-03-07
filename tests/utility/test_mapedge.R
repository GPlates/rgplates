library(rgplates)

# basic argumentation
expect_silent(sfEdge <- mapedge())
expect_true(inherits(sfEdge, "sfc"))

# with sp
library(sp)
expect_silent(spEdge <- mapedge(out="sp"))
expect_true(inherits(spEdge, "Spatial"))


