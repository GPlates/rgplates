library(sp)

source(file.path(wd, "rgplates/tests/aux/old_rec.R"))


dat <- chronosphere::fetch("pbdb", ver="20220510", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)

# 1. taxonomic filtering
	# filter records not identified at least to genus
	dat <-dat[dat$accepted_rank %in% c("genus", "species"),]

	# omit non-informative genus entries
	dat <- dat[dat$genus!="", ]

	#A. phyla
	marineNoPlant <- c("",
		"Agmata",
		"Annelida",
		"Bilateralomorpha",
		"Brachiopoda",
		"Bryozoa",
		"Calcispongea",
		"Chaetognatha",
		"Cnidaria",
		"Ctenophora",
		"Echinodermata",
		"Entoprocta",
		"Foraminifera",
		"Hemichordata",
		"Hyolitha",
		"Mollusca",
		"Nematoda",
		"Nematomorpha",
		"Nemertina",
		"Onychophora",
		"Petalonamae",
		"Phoronida",
		"Platyhelminthes",
		"Porifera",
		"Problematica",
		"Rhizopodea",
		"Rotifera",
		"Sarcomastigophora",
		"Sipuncula",
		"Uncertain",
		"Vetulicolia",
		""
	)

	# which rows?
	bByPhyla <- dat$phylum%in% marineNoPlant

	# the other
		noNeed <- dat[!bByPhyla,]
		needPhylum <- dat[bByPhyla,]

	#B. classes
	#	levels(factor(noNeed$class))
		needClass <- c(
			"Acanthodii",
			"Actinopteri",
			"Actinopterygii",
			"Agnatha",
			"Cephalaspidomorphi",
			"Chondrichthyes",
			"Cladistia",
			"Coelacanthimorpha",
			"Conodonta",
			"Galeaspida",
			"Myxini",
			"Osteichthyes",
			"Petromyzontida",
			"Plagiostomi",
			"Pteraspidomorphi",
			# here come the Arthropods
			"Artiopoda",
			"Branchiopoda",
			"Cephalocarida",
			"Copepoda",
			"Malacostraca",
			"Maxillopoda",
			"Megacheira",
			"Merostomoidea",
			"Ostracoda",
			"Paratrilobita",
			"Pycnogonida",
			"Remipedia",
			"Thylacocephala",
			"Trilobita",
			"Xiphosura"
		)
		
		# which rows?
		bNeedClass <- dat$class %in% needClass

	#C.  mammals
	#	mammals <- dat[dat$class=="Mammalia",]
	#	levels(factor(mammals$order))

		needMammalOrd <- c("Cetacea", "Sirenia")

		# which rows?
		bMammalOrder <- dat$order %in% needMammalOrd

		# the carnivores
		carnivores <- dat[dat$order=="Carnivora",]
		levels(factor(carnivores$family))

		needFam <- c("Otariidae", "Phocidae", "Desmatophocidae")

		# which rows?
		bNeedMamFam<- dat$family%in%needFam

	# D. Reptiles
	#	reptiles <- dat[dat$class=="Reptilia",]
	#	levels(factor(reptiles$order))

		needReptOrd<-c(
			"Eosauropterygia",
			"Hupehsuchia",
			"Ichthyosauria",
			"Placodontia",
			"Sauropterygia",
			"Thalattosauria"
		)
		
		# which rows?
		bRept <- dat$order%in%needReptOrd

	# E. turtles 
	#	turtles <- dat[dat$order=="Testudines",]
	#	levels(factor(turtles$family))
	
	# Chelonioidea turtles
	needTurtleFam <- c(
		"Cheloniidae",
		"Protostegidae",
		"Dermochelyidae",
		"Dermochelyoidae",
		"Toxochelyidae",
		"Pancheloniidae"
	)

	# which rows?
	bTurtle <- dat$family%in%needTurtleFam

	# subset the data
	dat <- dat[
		bByPhyla |
		bNeedClass |
		bMammalOrder |
		bNeedMamFam |
		bRept | 
		bTurtle
		, ]


	# resolve the potential homonymy problem
	dat$clgen <- paste(dat$class, dat$genus)

################################################################################
# 2. filter by environment
	levels(factor((dat$environment)))

	omitEnv <- c(
		"\"floodplain\"",
		"alluvial fan",
		"cave",
		"\"channel\"",
		"channel lag" ,
		"coarse channel fill",
		"crater lake",
		"crevasse splay",
		"dry floodplain",
		"delta plain",
		"dune",
		"eolian indet.",
		"fine channel fill",
		"fissure fill",
		"fluvial indet.",
		"fluvial-lacustrine indet.",
		"fluvial-deltaic indet.",
		"glacial",
		"interdune",
		"karst indet.",
		"lacustrine - large",
		"lacustrine - small",
		"lacustrine delta front",
		"lacustrine delta plain",
		"lacustrine deltaic indet.",
		"lacustrine indet.",
		"lacustrine interdistributary bay",
		"lacustrine prodelta",
		"levee",
		"loess",
		"mire/swamp",
		"pond",
		"sinkhole",
		"spring",
		"tar",
		"terrestrial indet.",
		"wet floodplain"
	)

	dat<-dat[!dat$environment%in%omitEnv, ]


# finally omit unlithified sediments
	dat <- dat[dat$lithification1!="unlithified",]

	
# resolving remaining marine environmental variables (not for this, but for additional analyses) 
	suppressPackageStartupMessages(library(divDyn))
 	#load from divDyn
 	data(keys)

 	# lithology
	dat$lith<-categorize(dat$lithology1,keys$lith)

	# batyhmetry
	dat$bath <- categorize(dat$environment,keys$bath) 

	# grain size
	dat$gra <- categorize(dat$lithology1,keys$grain) 

	# reef or not?
	dat$reef <- categorize(dat$environment, keys$reef) 
	dat$reef[dat$lith=="clastic" & dat$environment=="marine indet."] <- "non-reef" # reef or not?/2

	# onshore - offshore
	dat$depenv <- categorize(dat$environment,keys$depenv) 

################################################################################
# 3. stratigraphic resolution
	# time scales
	data(stages)
	data(tens)
	
	# rename some names so they are the same (Cosmetic changes, needed for the divDyn-analyses)
	colnames(tens)[colnames(tens)=="X10"]<-"name"
	colnames(stages)[colnames(stages)=="stage"]<-"name"
#####################################-------------------------------------------
# do the same for the stages
# B. the stg entries (lookup)
		stgMin<-categorize(dat[,"early_interval"],keys$stgInt)
		stgMax<-categorize(dat[,"late_interval"],keys$stgInt)

		# convert to numeric
		stgMin<-as.numeric(stgMin)
		stgMax<-as.numeric(stgMax)

	# empty container
		dat$stg <- rep(NA, nrow(dat))

	# select entries, where
		stgCondition <- c(
		# the early and late interval fields indicate the same stg
			which(stgMax==stgMin),
		# or the late_intervarl field is empty
			which(stgMax==-1))

	# in these entries, use the stg indicated by the early_interval
		dat$stg[stgCondition] <- stgMin[stgCondition]

	# NOT IN THE PRE-SILURIAN
################################################################################
# 2. Reconstruction of paleocoordinates to stage midpoints
################################################################################
# Reconstructions are implemented in the package 'rgplates'
library(rgplates)
# The PaleoDEMs
dems<- chronosphere::fetch("paleomap", "dem", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)

# the ages of the dems
names(dems)

# This information is specific to the time scale that we are using, so let's save it there!
stages$map <- matchtime(names(dems), stages$mid)
# Let's separate the collection-level information from the occurrences
collections <- dat[ , 
	c(NULL
		, "collection_no" # unique id
		, "lng" # current coordinates
		, "lat" # current coordinates
		, "stg" # thetemporal bin
		, "paleolat" # the original paleocoord (for comparison)
		, "paleolng" # the original paleocoord (for comparison)
	)
]

# make it collection-level
collections <- unique(collections)

# it takes a lot of time to reconstruct the paleocoordinates with the web application
# for this reason, rgplates has an offline method for reconstructions, which uses the console interface of GPlates
# However for that to work we need plate reconstruction model files. These can be acquired from chronosphere.
# For the PaleoMAP project this is:
model <- chronosphere::fetch("paleomap", "model", datadir=file.path(wd, "data/chronosphere"), verbose=FALSE)

# to effectively reconstruct the coordinates we need to put the target ages to our tables
# for every stg id we have a mapage, we just need to copy this from the time scale table to the collections
collections$stg_map<- stages$map[collections$stg] # based on the rule that stg 5 is in the 5th row!

# it is also a good idea to do a bit of cleaning
# 1. We want to focus on those data the have stage assignments!
collections <- collections[!is.na(collections$stg), ]

# and valid present-day coordinates
collections <- collections[!is.na(collections$lat) & !is.na(collections$lng), ]

# weird entries? 
range(collections$lng)
range(collections$lat)
collections <- collections[collections$lat<=90, ]

################################################################################
# Actual test starts here

################################################################################
# the plateperiod = FALSE

# now we can execute the reconstruction, this takes a couple of minutes (3-5). 
paleoCoords_old <- reconstruct_old(
	collections[, c("lng", "lat")] 
	, age = collections$stg_map 
	, model=model 
	, enumerate=FALSE 
	, plateperiod=FALSE 
)

paleoCoords_new <- reconstruct(
	collections[, c("lng", "lat")] 
	, age = collections$stg_map 
	, model=model 
	, enumerate=FALSE 
	, plateperiod=FALSE 
)

library(tinytest)

expect_equal(paleoCoords_old, paleoCoords_new)


################################################################################
# the plateperiod = TRUE

# now we can execute the reconstruction, this takes a couple of minutes (3-5). 
paleoCoords_old <- reconstruct_old(
	collections[, c("lng", "lat")] 
	, age = collections$stg_map 
	, model=model 
	, enumerate=FALSE 
	, plateperiod=TRUE 
)


paleoCoords_new <- reconstruct(
	collections[, c("lng", "lat")] 
	, age = collections$stg_map 
	, model=model 
	, enumerate=FALSE
	, plateperiod=TRUE
)

expect_equal(paleoCoords_old, paleoCoords_new)
