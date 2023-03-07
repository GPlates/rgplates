
# rgplates <img src="man/figures/logo.png" align="right" />

[![](https://img.shields.io/badge/devel%20version-0.2.1-green.svg)](https://github.com/adamkocsis/rgplates)
[![](https://www.r-pkg.org/badges/version/rgplates?color=blue)](https://cran.r-project.org/package=rgplates)
[![](http://cranlogs.r-pkg.org/badges/grand-total/rgplates?color=yellow)](https://cran.r-project.org/package=rgplates)
[![CRAN
checks](https://badges.cranchecks.info/summary/rgplates.svg)](https://cran.r-project.org/web/checks/check_results_rgplates.html)

#### R interface for the *GPlates Web Service* and the *GPlates Application*

## About

Functions to the [GPlates Web Service](https://gws.gplates.org/) and the
[GPlates Desktop Application](https://www.gplates.org/) allow users to
reconstruct past positions of geographic entities based on user-selected
rotation models without leaving the R running environment.

The **online reconstruction** (with the [GPlates Web
Service](https://gws.gplates.org/)) makes the rotation of static plates,
coastlines, and a low number of geographic coordinates available using
nothing but an internet connection.

The **offline reconstruction** requires an external installation of the
GPlates Desktop Application, but allows the efficient batch rotation of
thousands of coordinates, Simple Features
([`sf`](https://cran.r-project.org/package=sf)) and Spatial
([`sp`](https://cran.r-project.org/package=sp)) objects with custom
reconstruction trees and partitioning polygons. Examples of such plate
tectonic models are accessible via the
[chronosphere-portal](http://www.chronosphere-portal.org/).

-----

## Notes

#### History

The functions here were originally developed and published as part of
the [chronosphere](https://cran.r-project.org/package=chronosphere) R
package. For better compliance with [UNIX
philosophy](https://en.wikipedia.org/wiki/Unix_philosophy) and more
efficient distribution/development, the original chronosphere has been
broken up to three R packages:

  - `rgplates`: functions related to tectonic reconstructions.
  - `via`: Virtual Arrays for efficient organisation of high-dimenional
    data.
  - `chronosphere`: version-controlled data distribution.

This is a beta version, and like R, comes with absolutely no warranty.

#### Aim

The package was developed to make R-based analyses that require
paloecoordinate rotations easier to implement, i.e. for applying a
rotation model - but not as a replacement of tools dedidacted to complex
paleogeographic reconstruction and model development. For more complex
analyses you are more than welcome to check out
[pyGPlates](https://www.gplates.org/docs/pygplates/), the awesome python
module developed by the GPlates team.