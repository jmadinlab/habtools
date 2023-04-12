
<!-- README.md is generated from README.Rmd. Please edit that file -->

# habtools

<!-- badges: start -->
<!-- badges: end -->

The goal of `habtools` is to collate tools for 3D meshes and digital
elevation models (DEM) targeted at biologists and ecologists. Tools
calculate metrics like surface area, rugosity, fractal dimension, height
range, convexity, sphericity, second moments of volume and more. The
initial set of tools came from two research papers. Zawada et al. (2019)
examined morphology of coral colony laser scans. Torres-Pulliza et
al. (2020) examined complexity of coral reef structure. It is hoped that
the number of tools and contributors will grow through time.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmadinlab/habtools")
```

## Examples

There are currently two data set accompanying this package. `horseshoe`
is a digital elevation model of a reef, and `mcap` is a 3D mesh of a
coral growing on a reef.

The following example calculates height range, rugosity and fractal
dimension of a 2 x 2 m plot of reef using the height range method
developed in Torres-Pulliza et al. (2020).

``` r
library(habtools)
library(raster)
#> Loading required package: sp
plot(horseshoe)
```

<img src="man/figures/README-example1-1.png" width="100%" />

``` r
# height range
hr(horseshoe, x = -470, y = 1266, L = 2)
#> [1] 1.059676

# rugosity; note that rugosity will decrease with grain (L0)
rg(horseshoe, x = -470, y = 1266, L = 2, L0 = 0.125)
#> [1] 1.430732

# fractal dimension
fd(horseshoe, x = -470, y = 1266, L = 2, lvec = c(0.25, 0.5, 1, 2), plot = TRUE, method = "hvar")
```

<img src="man/figures/README-example1-2.png" width="100%" />

    #> [1] 2.337396

The next example calculates height range, rugosity and fractal dimension
for a 3D mesh of a coral colony. Because meshes can have more than one
`z` coordinate for a given `xy` (i.e., they have overhangs), we advise
the cube counting fractal dimension method presented in Zawada et
al. (2019).

``` r
# height range
hr(mcap)
#> [1] 0.2185397

# rugosity
rg(mcap, L0 = 0.045)
#> [1] 2.449289

# fractal dimension
fd(mcap, lvec = c(0.045, 0.09, 0.18, 0.5), plot = TRUE, method = "cubes")
```

<img src="man/figures/README-example2-1.png" width="100%" />

    #> [1] 2.158836
