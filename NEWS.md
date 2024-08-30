# Change log of the R package 'rgplates'

# rgplates 0.4.2 (pre) - 2024-08-30

### Added
- Support for `SpatRaster` class objects (package `terra`). The `terra` extension is now added as a suggested package.

### Fixed

- Bug with the offline method: GPlates 2.5.0 under Windows could not be reached because the default path resolved in the `gplates.exe.local` file.

# rgplates 0.4.1 - 2024-08-19

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13348606.svg)](https://doi.org/10.5281/zenodo.13348606) - "The Plates Be Driftin'"  

### Added
- The `checkgws()`, `getgws()` and `setgws()` functions that allow the customization of the remote location of the GPlates Web Service.

### Changed
- Docs updated to reflect repo transfer to https://github.com/gplates

* * *

# rgplates 0.4.0 - 2024-02-02

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10614722.svg)](https://doi.org/10.5281/zenodo.10614722) - "Wondering Continents"  

### Added
- New suggested package 'httr2' to be used with the online method (see changed), which allows the customized usage of HTTP requests.
- The `from` argument for `reconstruct()`, providing explicit support for forward reconstruction. The argumentation `from!=0, age=0 (default)`, calculates present-day coordinates of past coordinates. The argumentation `from!=0, age!=0` calculates shifted positions of coordinates at different times (`age`) than their original (`from`).
- Support for missing values (`NA`) for the point reconstructions, which no longer trigger an error, but are rather propagated through the pipeline as missing values. 
- The `warn` argument of `reconstruct()`, allowing users to switch off warnings that indicate that paleocoordinates are off the partitioning polygons.
- The `anchor` argument of `reconstruct()`, allowing users to change the anchor plate for the online submodule. This is necessary for toggling the paleomagnetic reference frame of the `"TorsvikCocks2017"` model in the GWS. 
- The former `plateperiod` (now: `validtime`) argument is usable with the GPlates Web Service and the online reconstruction method.

### Changed
- Online reconstruction method (with GWS) now uses the HTTP POST method for points and sends coordinates as form data. Internals were changed accordingly. This allows the processing of theoretically unlimited number of coordinates.
- Online reconstruction method (with GWS) is no longer bound to integer ages.
- Point reconstruction results (`matrix` and `data.frame` objects) now consistently have the column names `paleolong` and `paleolat` for past coordinates, and `long` and `lat` for present-day coordinates.
- The `plateperiod` argument was renamed to `validtime` for better semantic compatibility with the GPlates Web Service.

### Fixed
- Online reconstruction was not working when the package was not attached due to issue of the checking of the valid input.

### Deleted
- The 'chunk' argument of `reconstruct()` was deprecacted and is scheduled for omission.

* * *

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
