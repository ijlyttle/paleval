
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

This will have to go into a design-document eventually, but for now,
I’ll “puke” this here.

For the purpose of this package, following ggplot2 usage, we define:

  - **`pev_fcont`**: A (palette) function that, when called with a
    numeric vector with values between 0 and 1, returns the
    corresponding (hex-code) values.

  - **`pev_fdisc`**: A (palette) function that, when called with a
    single integer argument (the number of levels in the scale), returns
    the (hex-code) values that they should take. Such a function will
    have a maximum for the argument.

These follow the `palette` argument for
[`ggplot2::continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html)
and
[`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

There are ways to create continuous-palette functions:

  - hcl, etc. parameters, following the colorspace framework.
    `pev_cont_hcl()`
  - composing two continuous-palette functions to create (presumably) a
    diverging palette-function. It’s up to you to make sure the
    constituent palettes “meet in the middle”. `pev_cont_compose()`
  - rescaling an existing continuous-palette function. It may make sense
    to “zoom-in”, but “zooming-out” could get you into trouble.
    `pev_cont_rescale()`

There are ways to create discrete-palette functions:

  - discretizing a contiuous-palette function, specifying number of
    colors and discretization method (e.g. panels or posts).
    `pev_disc_cont()`
  - supplying a vector of hex-codes. `pev_disc_hex()`

As well, it would be useful to have a function that, given an
`fpal_discrete`, returns the maximum-index.

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

``` r
library("paleval")

fcont <- pev_fcont("Dynamic") # continuous palette-function, from colorspace
fdisc <- pev_fdisc(fcont, n = 7, method = "panel") # discrete palette-function
pev_fpal_to_hex(fdisc) # get the hex-colors
#> [1] "#E396A0" "#D796D0" "#9FA8E2" "#4CB9CC" "#50BE9B" "#97B56C" "#CBA56E"

data_sep <- pev_data_separation(fdisc)
print(data_sep)
#> # A tibble: 196 x 5
#>    cvd   index_a hex_a   hex_b   distance
#>    <chr>   <int> <chr>   <chr>      <dbl>
#>  1 none        1 #E396A0 #E396A0      0  
#>  2 none        1 #E396A0 #D796D0     14.9
#>  3 none        1 #E396A0 #9FA8E2     26.4
#>  4 none        1 #E396A0 #4CB9CC     53.6
#>  5 none        1 #E396A0 #50BE9B     53.0
#>  6 none        1 #E396A0 #97B56C     45.3
#>  7 none        1 #E396A0 #CBA56E     27.1
#>  8 none        2 #D796D0 #E396A0     14.9
#>  9 none        2 #D796D0 #D796D0      0  
#> 10 none        2 #D796D0 #9FA8E2     18.3
#> # … with 186 more rows

pev_gg_separation(data_sep)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Code of Conduct

Please note that the ‘paleval’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
