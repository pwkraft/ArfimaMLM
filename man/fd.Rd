\name{fd}
\alias{fd}
\alias{fd.default}
\alias{fd.numeric}
\title{
Estimate fractional differencing parameter and/or differenciate series for a given d-value
}
\description{
This is a wrapper function for the \code{\link[fractal:hurstSpec]{hurstSpec}}-function as well as the \code{\link[fracdiff:fracdiff]{fracdiff}}-package for internal use as part of the \code{\link[ArfimaMLM:arfimaMLM]{arfimaMLM}} function. The function estimates the fractional differencing parameter d of a series \code{x} and returns the fractionally differenced series. Alternatively, the  differencing parameter can be estimated externally and included as a numeric argument in \code{dval}. In this case, the function returns the fractionally differenced series without estimating the differencing parameter itself.
}
\usage{
fd(x, dval = "Hurst", ...)

## S3 method for class 'character'
fd(x, dval = "Hurst", ...)

## S3 method for class 'numeric'
fd(x, dval, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector or time series to be fractionally differenced.
}
  \item{dval}{
Call for a specific estimation method for the fractional differencing parameter in the \code{\link[fractal:hurstSpec]{hurstSpec}}-function (\code{"Hurst"}) or the \code{\link[fracdiff:fracdiff]{fracdiff}}-package (\code{"ML"}, \code{"GPH"}, or \code{"Sperio"}). Default is \code{"Hurst"}. Alternatively, \code{dval} can be a numeric argument that will be directly passed to \code{diffseries} to  calculate the fractionally differenced series according to the differencing parameter specified in \code{dval}. If a value of 0 is specified, the function will simply return the original series \code{x}.
}
  \item{\dots}{
Further arguments passed to  \code{\link[fractal:hurstSpec]{hurstSpec}}, \code{\link[fracdiff:fracdiff]{fracdiff}}, \code{\link[fracdiff:fdGPH]{fdGPH}},
\code{\link[fracdiff:fdSperio]{fdSperio}}, or \code{\link[fracdiff:diffseries]{diffseries}}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{series}{Fractionally differenciated series.}
\item{estimator}{Method used to estimate differencing parameter (\code{"Hurst"}, \code{"ML"}, \code{"GPH"}, or \code{"Sperio"})), prompts \code{"external"} if a differencing parameter was provided as numeric argument for \code{dval}.}
\item{value}{Estimated differencing parameter, returns the \code{dval} input if a numeric argument was provided for \code{dval}.}
}
\references{
See those in \code{\link[fractal:hurstSpec]{hurstSpec}}, \code{\link[fracdiff:fracdiff]{fracdiff}}, \code{\link[fracdiff:fdGPH]{fdGPH}},
\code{\link[fracdiff:fdSperio]{fdSperio}}, and \code{\link[fracdiff:diffseries]{diffseries}}.
}
\author{
Patrick Kraft
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[fractal:hurstSpec]{hurstSpec}}, \code{\link[fracdiff:fracdiff]{fracdiff}}, \code{\link[fracdiff:fdGPH]{fdGPH}},
\code{\link[fracdiff:fdSperio]{fdSperio}}, \code{\link[fracdiff:diffseries]{diffseries}}
}
\examples{
require(fractal)
require(fracdiff)

set.seed(123)
series=fracdiff.sim(100, d=0.4, mu=10)$series

## S3 method for class 'character'
# estimates fractional differencing parameter d
# and differenciates series accordingly
series.fd1<-fd(series)
series.fd2<-fd(series, dval="ML")
series.fd3<-fd(series, dval="GPH")
series.fd4<-fd(series, dval="Sperio")

## S3 method for class 'numeric'
# differenciates series according to 
# externally provided differencing parameter
series.fd5<-fd(series, dval=0.4)
}
\keyword{ts}
\keyword{manip}