# Change log of the R package 'rgplates'

# rgplates 0.4.0 - 2024-01-30

### Added
- New suggested package 'httr2' to be used with the online method (see changed).

### Changed
- Online reconstruction method (with GWS) now uses the HTTP POST method for points and send coordinates as form data. Internals were changed accordingly.

### Fixed
- Online reconstruction was not working when the package was not attached, due to issue of the checking of the valid input.

### Deleted
- The 'chunk' agrument of `reconstruct()` was deprecacted.

# rgplates 0.3.2 - 2023-09-06

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8321157.svg)](https://doi.org/10.5281/zenodo.8321157)

### Fixed
- Bug fix for windows paths in the offline reconstruction methods 

# rgplates 0.3.1 - 2023-08-09 

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8229554.svg)](https://doi.org/10.5281/zenodo.8229554)

### Added
- The `gws` data object that includes details about the reconstructable features in the GPlates Web services
- Routines to check user entry to the GPlates Web Service with the `gws` Object.
- The `platemodel` class now accepts feature descriptions as a `data.frame`. This allows the inclusion of valid age ranges for the reconstruction of feature collections.
- Simon Williams as contributor, as his code was the basis for accessing the GWS
- import of `utils::data()`

### Fixed
- Minor bug that did not allow topological features to be reconstructed.  
- Minor bug that occurred when `enumerate=FALSE` and cases when the target `age` included `NA`s
- An error that occurred when `plateperiod=TRUE`, the target age went beyond the duration of the plates and no coordinates were returned by the GPlates Desktop Application. 

* * *


# rgplates 0.3.0 - 2023-06-29 

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8098723.svg)](https://doi.org/10.5281/zenodo.8098723)

### Added 
- The `platemodel` class now accepts multiple feature collections. These can be recontructed by setting the `x` argument of `recontruct()`.  

### Changed
- The interface of the `reconstruct()` function now matches that of the GPlates web service. Use `"static_polygons"` instead of `"plates"`. 
- The `plateperiod` argument of `reconstruct()` now defaults to `TRUE`
- Documentation to reflect models avialble through the GPlates web service
- Changed default method of online reconstruction to `"MERDITH2021"`

### Removed
- unnecessary imports from `utils`

* * *

# rgplates 0.2.1 - 2023-03-08 


[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8094071.svg)](https://doi.org/10.5281/zenodo.8094071)


### Added
- the `mapedge()` function
- function website assets

### Changed
- example data switched to PaleoMAP v3

* * *

# rgplates 0.2.0 - 2022-09-19



[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8094042.svg)](https://doi.org/10.5281/zenodo.8094042)



### Added
- Package now depends on the sustainable 'sf'-based interface to GPLates
- Defense against non-integer reconstruction dates for the online method.
- Support for 'sf' type objects for the online reconstruction method 
- defense against decimal ages for online reconstruction method.
- Support for all 'Spatial*' objects (sp) in the offline reconstruction method
- The 'gmeta' argument of the local reconstruction submodule and sf output.

### Changed
- All spatial outputs now default to sf instead of Spatial* types
- online reconstruction method now suggests the 'geojsonsf' extension package for reading in features

### Removed
- dependency of deprecated 'rgdal' extension
- Support for polygons for the online reconstruction method. 

### Known issues
- Matrix-based reconstructinos do not accept missing values. 

* * *

# rgplates 0.1.0 - 2021-05-11 

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8093991.svg)](https://doi.org/10.5281/zenodo.8093991)

### Changed
- Bug fix for spaces in usernames on Windows machines.


* * *

# rgplates 0.1.0 - 2021-05-09 


### Added 
- material copied over from chronosphere 0.4.1 
- reconstruct(model=NULL) argumentation.

### Changed
- platemodel data example is manually extracted
- reconstruct example replaced with model=NULL
