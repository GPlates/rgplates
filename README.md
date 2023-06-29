
# rgplates

[![](https://img.shields.io/badge/devel%20version-0.3.0-green.svg)](https://github.com/adamkocsis/rgplates)
[![](https://www.r-pkg.org/badges/version/rgplates?color=orange)](https://cran.r-project.org/package=rgplates)
[![](http://cranlogs.r-pkg.org/badges/grand-total/rgplates?color=yellow)](https://cran.r-project.org/package=rgplates)
[![CRAN
checks](https://badges.cranchecks.info/summary/rgplates.svg)](https://cran.r-project.org/web/checks/check_results_rgplates.html)

R interface for the GPlates Web Service and Desktop Application

See the webpage of the package for more info and tutorials:
<https://adamkocsis.github.io/rgplates/>

(Ádám T. Kocsis and Nussaïbah B. Raja) Query functions to the GPlates
<https://www.gplates.org/> Desktop Application and the GPlates Web
Service <https://gws.gplates.org/> allow users to reconstruct past
positions of geographic entities based on user-selected rotation models
without leaving the R running environment. The online method (GPlates
Web Service) makes the rotation of static plates, coastlines, and a low
number of geographic coordinates available using nothing but an internet
connection. The offline method requires an external installation of the
GPlates Desktop Application, but allows the efficient batch rotation of
thousands of coordinates, Simple Features (sf) and Spatial (sp) objects
with custom reconstruction trees and partitioning polygons. Examples of
such plate tectonic models are accessible via the chronosphere-portal
<https://cran.r-project.org/package=chronosphere>. This R extension is
developed under the umbrella of the DFG (Deutsche
Forschungsgemeinschaft) Research Unit TERSANE2 (For 2332, TEmperature
Related Stressors in ANcient Extinctions).

The functions here were removed from the chronosphere R package
(v0.4.1), for better compliance with UNIX-principles and more efficient
distribution/development.

This is a beta version.

See the blog entry below for an example application:
