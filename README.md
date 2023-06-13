
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BertopicR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of BertopicR is to allow R users to access bertopic’s topic
modelling suite in R.

The package currently installs an exact version of bertopic - 0.14.0,
this should make it easier to maintain the package over time and prevent
new users from running into install errors.

## Installation

Before installing bertopic make sure that you have miniconda installed,
if you don’t:

``` r
library(reticulate) #install.packages("reticulate") if you don't have this already or aren't sure how to install.

reticulate::install_miniconda()
```

Once you have reticulate and miniconda installed, you can then install
the development version of BertopicR from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("jpcompartir/BertopicR")

library(BertopicR)

#Check your environment has been loaded correctly and bertopic has been installed:
BertopicR::check_python_dependencies()
```

If you receive the message: “bertopic not in installed packages of
current environment…” run:

``` r
BertopicR::install_python_dependencies()
```

## Example

``` r
#TODO
```
