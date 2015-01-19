\name{extractVars}
\alias{extractVars}
\title{
Extract and order variables from formula
}
\description{
This is an internal function used within \code{\link{arfimaMLM}} and \code{\link{arfimaOLS}}. It is used to extract the variables marked with the suffixes \code{.fd}, \code{.xdif}, and \code{.ydif} from \code{formula} as well as \code{.mean} from \code{ecmformula} and create variable lists for the respective types of data manipulations.
}
\usage{
extractVars(formula,ecmformula=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
   An object of the class "\code{\link{formula}}" that specifies a multilevel or a simple linear model. See \code{\link{lmer}}, \code{\link{lm}}, \code{\link{arfimaMLM}}, and \code{\link{arfimaOLS}} for details.
}
  \item{ecmformula}{
  An object of the class "\code{\link{formula}}" that specifies a simple linear model. See \code{\link{lm}}, \code{\link{arfimaMLM}}, and \code{\link{arfimaOLS}} for details.
}
}
\details{
}
\value{
The function returns a list of character vectors containing variable names selected for subsequent data manipulation:
\item{mean}{List of variables selected to calculate level means (i.e. variables originally augmented with suffixes \code{.fd}, \code{.xdif}, \code{ydif}, and \code{.mean}).}
\item{fd}{List of variables selected for fractional differencing (i.e. variables originally augmented with suffixes \code{.fd}, and \code{ydif}).}
\item{xdif}{List of variables originally augmented with the suffix \code{.xdif}.}
\item{xdif}{List of variables originally augmented with the suffix \code{.ydif}.}
}
\author{
Patrick Kraft
}
\note{
%%  ~~further notes~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[ArfimaMLM:arfimaMLM]{arfimaMLM}}, \code{\link[ArfimaMLM:arfimaOLS]{arfimaOLS}}, \code{\link[ArfimaMLM:ArfimaMLM]{ArfimaMLM}}.
}
\examples{

f1 <- y ~ x1.xdif + x2 + z1.fd + z2
f2 <- y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + (1+x1.xdif|time)
f3 <- y.mean ~ x1.mean

extractVars(formula = f1)
extractVars(formula = f2, ecmformula = f3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{internal}