
# tidylandmark

<!-- badges: start -->
[![R-CMD-check](https://github.com/thatoneguy006/tidylandmark/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thatoneguy006/tidylandmark/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/thatoneguy006/tidylandmark/graph/badge.svg)](https://app.codecov.io/gh/thatoneguy006/tidylandmark)
<!-- badges: end -->

Provides a tidy framework for landmark survival analysis, facilitating the 
construction of landmark-specific risk sets, incorporation of longitudinal 
covariates, and estimation of associations with time-to-event outcomes. 
Supports covariate estimation using last observation carried forward (LOCF) or 
linear mixed-effects models (LME), alongside Cox proportional hazards and 
Fine-Gray competing risks regression. Offers a consistent and reproducible 
workflow for conducting analyses across multiple landmark times.

## Installation

### CRAN
Coming soon...

### Development
``` r
# install.packages("remotes")
remotes::install_github("thatoneguy006/tidylandmark")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidylandmark)
## basic example code
```

