
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paleval

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/paleval)](https://CRAN.R-project.org/package=paleval)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/ijlyttle/paleval.svg?branch=master)](https://travis-ci.org/ijlyttle/paleval)
[![Codecov test
coverage](https://codecov.io/gh/ijlyttle/paleval/branch/master/graph/badge.svg)](https://codecov.io/gh/ijlyttle/paleval?branch=master)
<!-- badges: end -->

The goal of paleval is to help you evaluate the effectiveness of color
palettes and color maps. It builds from the
[colorspace](http://colorspace.r-forge.r-project.org) package, used to
design color maps, and the [farver](https://github.com/thomasp85/farver)
package, used to evaluate the perceptual difference between two colors.

For the purpose of this package, we define:

  - **palette**: a collection of colors intended for use with
    categorical data. Order is used for familiarity, rather than to
    imply order among observations of the encoded variable.
    Interpolation within a palette is not required to be a well-formed
    idea.

  - **map**: an ordered collection of colors intended for use with
    numerical data, or for data where we wish to imply order among
    observations of the encoded variable. We expect to be able to
    interpolate within a map.

## Installation

You can install the development version of paleval from
[GitHub](https://github.com/ijlyttle/paleval) with:

``` r
devtools::install_github("ijlyttle/paleval")
```

<!--
You can install the released version of paleval from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("paleval")
```
-->

## Example

Forthcoming.

## Code of Conduct

Please note that the ‘paleval’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
