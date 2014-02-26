\name{ArfimaMLM-package}
\alias{ArfimaMLM-package}
\alias{ArfimaMLM}
\docType{package}
\title{
ARFIMA-MLM estimation for repeated cross-sectional data and pooled cross-sectional time series data
}
\description{
\code{ArfimaMLM} provides functions to facilitate the estimation of ARFIMA-MLM for repeated cross-sectional data and pooled cross-sectional time series data. The estimation procedure uses double filtering with ARFIMA methods to account for autocorrelation in longer RCS followed by the use of multilevel modeling (MLM) to estimate both aggregate- and individual-level parameters simultaneously.
}
\details{
\tabular{ll}{
Package: \tab ArfimaMLM\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2014-02-25\cr
License: \tab GPL-2\cr
}
The main function of the package is \code{\link[ArfimaMLM:arfimaMLM]{arfimaMLM}}, which implements ARFIMA and multilevel models on a repeated cross-sectional dataset as described by Lebo and Weber (forthcoming). Furthermore, the function \code{\link[ArfimaMLM:arfimaOLS]{arfimaOLS}} uses the same initial procedures but estimates a simple linear model instead of the multilevel model. The package also includes \code{\link[ArfimaMLM:arfimaPrep]{arfimaPrep}}, which prepares a dataset for subsequent analyses according to the Arfima-MLM framework without estimating the final model itself. \code{\link[ArfimaMLM:arfimaMLM]{fd}} is a wrapper function to estimate the fractional differencing parameter using \code{\link[fractal:hurstSpec]{hurstSpec}} of the \code{fractal}-package as well as procedures provided by the \code{\link[fracdiff:fracdiff]{fracdiff}}-package (via ML, GPH, and Sperio) and to differentiate the series accordingly (mainly for internal use in \code{arfimaMLM},\code{arfimaOLS}, and \code{arfimaPrep}).
}
\author{
Patrick Kraft, with contributions from Christopher Weber

Maintainer: Patrick Kraft <patrick.kraft@stonybrook.edu>
}
\references{
Lebo, M. and Weber, C. (forthcoming). An Effective Approach to the Repeated Cross Sectional Design. American Journal of Political Science
}
\seealso{
\code{\link[lme4:lme4]{lme4}}, \code{\link[fracdiff:fracdiff]{fracdiff}}, \code{\link[fractal:hurstSpec]{hurstSpec}}, 
\code{\link[ArfimaMLM:arfimaMLM]{arfimaMLM}}, \code{\link[ArfimaMLM:arfimaOLS]{arfimaOLS}}, \code{\link[ArfimaMLM:arfimaPrep]{arfimaPrep}}, \code{\link[ArfimaMLM:fd]{fd}}
}
\keyword{ package }