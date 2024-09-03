options(timeout = 5*60)

# ensure detachment
try(detach("package:rgplates"), silent=TRUE)

# attempt to do a simple point reconstruction
expect_silent(
	rec0 <- rgplates::reconstruct("coastlines", 0, model="MERDITH2021")
)


