Dasst
=====


[![Travis Build Status](https://travis-ci.org/hlozza/Dasst.svg?branch=master)](https://travis-ci.org/hlozza/Dasst)
[![CRAN release](https://www.r-pkg.org/badges/version-last-release/Dasst)](https://cran.r-project.org/package=Dasst)


`Dasst` provides methods and tools for reading, processing and writing 
files complying with the `DSSAT-CSM` fixed width format. `Dasst` enables
DSSAT-style AS Simple Tables. The `DSSAT-CSM` cropping system model
is described at J.W. Jones, G. Hoogenboomb, C.H. Porter, K.J. Boote,
W.D. Batchelor, L.A. Hunt, P.W. Wilkens, U. Singh, A.J. Gijsman,
J.T. Ritchie (2003) <https://doi.org/10.1016/S1161-0301(02)00107-7>.

## Installing the package

To install the latest version from `GitHub`:

```R
install.packages("devtools")
devtools::install_github("hlozza/Dasst")
```

## Quick tutorial

```R
library(Dasst)

### Example
data(plantGrowth)
plot(plantGrowth[[1]][,c("DAP","LAID")])
```

[Github](https://github.com/hlozza/Dasst)
Homero Lozza <homerolozza@gmail.com>
