\name{hurst}
\title{
Estimate fractional differencing parameter via Hurst
}
\description{
This is a slight modification of the \code{hurstSpec} function in the \code{fractal}-package for internal use in the ArfimaMLM package. The original package describes it as a function to estimate the Hurst parameter H of a time series by linear regression of the log(spectrum) versus log(frequency) with frequency points accumulated into boxes of equal width on a logarithmic scale and spectrum values averaged over each box. Note that for this modified version, any original series is differenced prior to estimation!
}
\usage{
hurst(x, method="standard", sdf.method="multitaper")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Numeric vector or time series to be fractionally differenced (a vector containing a uniformly-sampled real-valued time series).
}
  \item{method}{a character string indicating the method to be used in estimating the Hurst coefficient (H). Choices are:
  
\code{"standard"}: Regression of SDF estimate.

\code{"smoothed"}: Regression of block averages of the SDF estimate taken over dyadic partitions in frequency.

\code{"robinson"}: Robinson's SDF integration method.

Default: "standard".}
\item{sdf.method}{a character string denoting the method to use in estimating the SDF. Choices are \code{"direct"}, \code{"lag window"}, \code{"wosa"} (Welch's Overlapped Segment Averaging), \code{"multitaper"}. Default: \code{"multitaper"}.}
  \item{\dots}{
Further arguments passed to  \code{\link[fractal:hurstSpec]{hurstSpec}} (not fully functional yet).
}
}
\details{
See the documentation of the original function (\code{\link[fractal:hurstSpec]{hurstSpec}}) for further information.
}
\value{
\item{d}{Fractional differencing parameter estimated based on the Hurst exponent.}
}
\references{
See those in \code{\link[fractal:hurstSpec]{hurstSpec}}.
}
\author{
William Constantine and Donald Percival (original \code{\link[fractal:hurstSpec]{hurstSpec}}-package), modified for internal use as part of \code{arfimaMLM} by Patrick Kraft.
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[fracdiff:fracdiff]{fracdiff}}, \code{\link[fracdiff:fdGPH]{fdGPH}},
\code{\link[fracdiff:fdSperio]{fdSperio}}, \code{\link[fracdiff:diffseries]{diffseries}}
}
\examples{
require(fracdiff)

set.seed(123)
series=fracdiff.sim(100, d=0.4, mu=10)$series

d <- hurst(series)
}
\keyword{ts}
\keyword{internal}
