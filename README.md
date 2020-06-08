# This package is deprecated!

This package has been split into smaller packages, all available at http://github.com/pik-piam:

- mrcommons 
- mrfeed
- mrland
- mrmagpie
- mrplayground
- mrremind
- mrvalidation

Please use these packages instead as moinput is not further developed anymore.

# R moinput package

## Purpose and Functionality

The R-library moinput provides useful functions and a common structure to all the input data required to run models like MAgPIE and REMIND


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", rd3mod_repo = "http://www.pik-potsdam.de/rd3mod/R/"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("moinput")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r 
vignette("moinput")
```

## Travis CI Integration

[![Travis build status](https://travis-ci.com/pik-piam/moinput.svg?branch=master)](https://travis-ci.com/pik-piam/moinput)


## Questions / Problems

In case of questions / problems please contact Jan Dietrich <dietrich@pik-potsdam.de>.

## Citation

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3699594.svg)](https://doi.org/10.5281/zenodo.3699594)
