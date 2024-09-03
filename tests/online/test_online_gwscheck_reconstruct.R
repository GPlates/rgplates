# Unit tests to check that gws entries can be reconstrcuted propoerliy
# and whether the checking flags them to inappropriate
library(rgplates)
data(gws)

# a valid age
midAge <- (gws$from+gws$to)/2

# Valid entries:
# loop through all feature/model combinations
for(i in 1:nrow(gws)){
	expect_silent(one <- reconstruct(x=gws$feature[i], model=gws$model[i], age=midAge[i]))
	expect_true(inherits(one, "sf"))
}

# Invalid entries - beyond valid window
for(i in 1:nrow(gws)){
	expect_error(one <- reconstruct(x=gws$feature[i], model=gws$model[i], age=gws$from[i]+1))
}
