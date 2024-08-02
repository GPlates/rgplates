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


