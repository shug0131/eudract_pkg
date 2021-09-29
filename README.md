
<!-- badges: start -->

[![R-CMD-check](https://github.com/shug0131/eudraCT/workflows/R-CMD-check/badge.svg)](https://github.com/shug0131/eudract_pkg/actions)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/shug0131/eudract_pkg?branch=master&svg=true)](https://ci.appveyor.com/project/shug0131/eudract_pkg)
[![Codecov
eudract_pkg](https://codecov.io/gh/shug0131/eudract_pkg/branch/master/graph/badge.svg)](https://codecov.io/gh/shug0131/eudract_pkg?branch=master)
<!-- badges: end -->

## Historical CI

[![Travis-CI Build Status (not
active)](https://travis-ci.com/shug0131/eudraCT.svg?branch=master)](https://travis-ci.com/shug0131/eudraCT)
[![codecov eudraCT (not
active)](https://codecov.io/github/shug0131/eudraCT/branch/master/graphs/badge.svg)](https://codecov.io/github/shug0131/eudraCT)

<!-- README.md is generated from README.Rmd. Please edit that file -->

# eudract

The goal of eudract is to provide tools to easily produce summaries of
safety data from clinical trials that can easily be uploaded into
eudraCT or ClinTrials.gov .

## Installation

You can install from CRAN directly with

``` r
install.packages("eudract")
```

You can install the very latest version on github with:

``` r
install.packages("devtools")
devtools::install_github("shug0131/eudraCT/R/eudract")
```

## Documentation

<https://eudract-tool.medschl.cam.ac.uk> provides full documentation

Go and read the help pages within R

``` r
?eudract::safety_summary
#> starting httpd help server ... done
```
