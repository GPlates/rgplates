library(rgplates)

# this should return the factory default
expect_equal(default <- getgws(), rgplates:::gwsURL)

# checking the GWS - requires good instance running!
expect_message(ver <- checkgws())
expect_silent(ver <- checkgws(silent=TRUE))
expect_true(!is.na(ver))

# checking setting of URL with garbage entry
# neither should work
wrongURL <- "garbage"
expect_silent(setgws(wrongURL, check=TRUE, silent=TRUE))
expect_warning(setgws(wrongURL, check=TRUE))

# should not change the url
expect_equal(default, getgws())

# successful change
expect_silent(setgws(wrongURL, check=FALSE))
expect_equal(wrongURL, getgws())

# explicit check should fail
expect_warning(checkgws())
expect_silent(checkgws(silent=TRUE))

# version returned should be missing value
expect_warning(missing <- checkgws())
expect_true(is.na(missing))

# reset the gws
expect_message(setgws(reset=TRUE))
expect_silent(setgws(reset=TRUE, silent=TRUE))
expect_equal(default, getgws())


# check if local instance is there
something <- NULL
try(something <- scan("http://localhost:18000/reconstruct/reconstruct_points/?lons=95,142&lats=54,-33&time=140&model=MULLER2019", what=character(), quiet=TRUE), silent=TRUE)

if(is.null(something[1])){
	warning("Local GWS not functional.")
}else{
	expect_silent(setgws("http://localhost:18000/", check=FALSE))
	expect_silent(rec <- reconstruct("static_polygons", age=3, model="MERDITH2021"))
	expect_silent(setgws(reset=TRUE, check=FALSE))
}
