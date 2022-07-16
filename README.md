
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [fairadapt](https://dplecko.github.io/fairadapt/)

<!-- badges: start 
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R build status](https://github.com/dplecko/fairadapt/workflows/build/badge.svg)](https://github.com/dplecko/fairadapt/actions?query=workflow%3Abuild)
[![R check status](https://github.com/dplecko/fairadapt/workflows/check/badge.svg)](https://github.com/dplecko/fairadapt/actions?query=workflow%3Acheck)
[![pkgdown build status](https://github.com/dplecko/fairadapt/workflows/pkgdown/badge.svg)](https://github.com/dplecko/fairadapt/actions?query=workflow%3Apkgdown)
[![covr status](https://github.com/dplecko/fairadapt/workflows/coverage/badge.svg)](https://github.com/dplecko/fairadapt/actions?query=workflow%3Acoverage)
[![Codecov test coverage](https://codecov.io/gh/dplecko/fairadapt/branch/main/graph/badge.svg?token=8A0EL5N4RE)](https://app.codecov.io/gh/dplecko/fairadapt)
<!-- badges: end -->

The R-package `faircause` can be used for performing Causal Fairness
Analysis and implements the methods described in the paper [Causal
Fairness Analysis (Plecko & Bareinboim, 2022)](fairness.causalai.net).
We refer you to the manuscript for full theoretical details and the
methodology. Below we offer quick installation instructions and show a
worked example that can help the user get started.

## Installation

You can install `faircause` from this Github repository by using the
[devtools](cran.devtools.com) package:

``` r
devtools::install_github("dplecko/faircause")
```

Please note that `faircause` is currently at its first version
`0.0.0.9000`, meaning that is has not yet been thoroughly tested. Any
issues and bug reports would therefore be much appreciated.

## Example

<!-- example could be expanded to show bias before correction -->
<!-- add plot of adj.mat? visualization of how data changed? -->

We show an example of how to use the `faircause` package on the US
Government Census 2018 dataset collected by [American Community
Survey](). The dataset contains information on 204,309 employees of the
US government, including demographic information
![Z](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Z "Z")
(age, race, location, citizenship), education and work related
information
![W](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;W "W"),
and the yearly earnings
![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y").
The protected attribute
![X](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;X "X")
we consider in this case is gender
(![x_1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_1 "x_1")
male,
![x_0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;x_0 "x_0")
female).

A data scientist analyzing the Census dataset observes the following:

``` r
census <- fairadapt::gov_census # should change this to faircause
TV <- mean(census$salary[census$sex == "male"]) -
  mean(census$salary[census$sex == "female"])

TV
#> [1] 14011.28
```

In the first step the data scientist computed that the average disparity
in the yearly salary measured by the TV is

![ E\[Y \\mid x_1\] - E\[Y \\mid x_0\] = \\$ 14011.](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%20E%5BY%20%5Cmid%20x_1%5D%20-%20E%5BY%20%5Cmid%20x_0%5D%20%3D%20%5C%24%2014011. " E[Y \mid x_1] - E[Y \mid x_0] = \$ 14011.")
