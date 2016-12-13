#Dasst

The R package *Dasst* provides methods and tools for reading, processing and writing files complying with the DSSAT-CSM fixed width format.


## Installation
You can install on linux systems

```sh
R CMD INSTALL Dasst_0.1.0.tar.gz
```

On windows platforms, you can install a copy of the binary file 

```R
install.packages("Dasst_0.1.0.zip", repos=NULL)
```


## Usage

```R
library(Dasst)

### Example
data(plantGrowth)
plot(plantGrowth[1][,c("DAP","LAID")])
```


[R CRAN](https://cran.r-project.org/package=Dasst)
[Github](https://github.com/hlozza/Dasst)
Homero Lozza <homerolozza@gmail.com>
