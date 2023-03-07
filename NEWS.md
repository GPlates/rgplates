# Change log of the R package 'rgplates

## [0.2.1] - 2023-03-08 (build 5)
### Added
- the mapedge() function
- function website assets

### Changed
- example data switched to Paleomap 2016

## [0.2.0] - 2022-09-19 (build 4)
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

## [0.1.0] - 2021-05-11 (build 3)
### Changed
- Bug fix for spaces in usernames on Windows machines.


## [0.1.0] - 2021-05-11 (build 2)
### Added 
- material copied over from chronosphere 0.4.1 
- reconstruct(model=NULL) argumentation.

### Changed
- platemodel data example is manually extracted
- reconstruct example replaced with model=NULL
