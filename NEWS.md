# Change log of the R package 'rgplates'

# rgplates - 0.3.0 - 2023-06-28 
### Added 
- The `platemodel` class now accepts multiple feature collections. These can be recontructed by setting the `x` argument of `recontruct()`.  

### Changed
- The interface of the `reconstruct()` function now matches that of the GPlates web service. Use `"static_polygons"` instead of `"plates"`. 
- The `plateperiod` argument of `reconstruct()` now defaults to `TRUE`
- Documentation to reflect models avialble through the GPlates web service
- Changed default method of online reconstruction to `"MERDITH2021"`

# rgplates 0.2.1 - 2023-03-08 
### Added
- the `mapedge()` function
- function website assets

### Changed
- example data switched to PaleoMAP v3

* * *

# rgplates 0.2.0 - 2022-09-19
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
### Changed
- Bug fix for spaces in usernames on Windows machines.


* * *

# rgplates 0.1.0 - 2021-05-11 
### Added 
- material copied over from chronosphere 0.4.1 
- reconstruct(model=NULL) argumentation.

### Changed
- platemodel data example is manually extracted
- reconstruct example replaced with model=NULL
