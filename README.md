ArfimaMLM estimation for repeated cross-sectional data and pooled cross-sectional time series data in R
=======================================================================================================

Description
-----------
**ArfimaMLM** provides functions to facilitate the estimation of Arfima-MLM for repeated cross-sectional data and pooled cross-sectional time series data. The estimation procedure uses double filtering with ARFIMA methods to account for autocorrelation in longer RCS followed by the use of multilevel modeling (MLM) to estimate both aggregate- and individual-level parameters simultaneously.

* Package: ArfimaMLM
* Type: Package
* Version: 1.3
* Date: 2015-01-19
* License: GPL-2

The main function of the package is `arfimaMLM`, which implements ARFIMA and multilevel models on a repeated cross-sectional dataset as described by Lebo and Weber (2015). Furthermore, the function `arfimaOLS` uses the same initial procedures but estimates a simple linear model instead of the multilevel model. The package also includes `arfimaPrep`, which prepares a dataset for subsequent analyses according to the Arfima-MLM framework without estimating the final model itself. `fd` is a wrapper function to estimate the fractional differencing parameter using `hurstSpec` of the `fractal`-package as well as procedures provided by the `fracdiff`-package (via ML, GPH, and Sperio) and to differentiate the series accordingly (mainly for internal use in `arfimaMLM`, `arfimaOLS`, and `arfimaPrep`).


Authors
-------
Patrick Kraft, with contributions from Christopher Weber

Maintainer: Patrick Kraft 


References
----------
Lebo, M. and Weber, C. 2015. "An Effective Approach to the Repeated Cross Sectional Design." American Journal of Political Science 59(1): 242-258.
