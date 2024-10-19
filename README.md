
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [faircause](https://github.com/dplecko/CFA)

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
Fairness Analysis (Plecko & Bareinboim,
2024)](https://causalai.net/r90.pdf). We refer you to the manuscript for
full theoretical details. In this repository, you will find a range of
examples that demonstrate how to use Causal Fairness Analysis in
practice.

### Suggested Citation

To cite the paper, please use the following:

    @article{plecko2024CFA,
      title={Causal Fairness Analysis: A Causal Toolkit for Fair Machine Learning},
      author={Ple{\v{c}}ko, Drago and Bareinboim, Elias},
      journal={Foundations and Trends{\textregistered} in Machine Learning},
      volume={17},
      number={3},
      pages={304--589},
      year={2024},
      publisher={Now Publishers, Inc.}
    }

### Installation

You can install `faircause` from this Github repository by using the
[devtools](https://cran.r-project.org/web/packages/devtools/index.html)
package:

``` r
devtools::install_github("dplecko/CFA")
```

Please note that `faircause` is still under development (currently in
version `0.2.0`) and any debug reports or suggested fixes are welcome.

### How to use CFA

A number of vignettes demonstrating how to use the package can be found
on our [Github pages](https://dplecko.github.io/CFA/).

### Want to learn more about Causal Fairness Analysis?

For those interested in learning more about CFA, we suggest the
following resources:

<ol style="list-style-type: lower-roman; counter-reset: list;">
<li>
Reading the Causal Fairness Analysis paper, found
<a href="https://causalai.net/r90.pdf">here</a>,
</li>
<li>
Follow the series of
<a href="https://www.cs.columbia.edu/~dplecko/#teaching">lectures on
CFA</a> which were part of the COMSW-4775 course at Columbia Computer
Science,
</li>
<li>
Check our <a href="https://fairness.causalai.net/">ICML 2022
Tutorial</a>.
</li>
<li>
Check the vignettes on <a href="https://dplecko.github.io/CFA/">Github
pages</a> that demonstrate how to perform Causal Fairness Analysis in
practice.
</li>
</ol>
