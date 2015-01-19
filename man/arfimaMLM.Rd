\name{arfimaMLM}
\alias{arfimaMLM}
\alias{print.arfimaMLM}
\alias{print.summary.arfimaMLM}
\alias{summary.arfimaMLM}
\title{
ARFIMA-MLM estimation for repeated cross-sectional data and pooled cross-sectional time series data
}
\description{
Fits an ARFIMA-MLM to a repeated cross-sectional dataset or a pooled cross-sectional time series dataset. For the variables specified by the user, the function automatically implements the aggregation and fractional differencing of time/level variables as well as the necessary procedures to remove deterministic components from the dependent as well as the major independent variables.
}
\usage{
arfimaMLM(formula, data, timevar
          , d = "Hurst", arma = NULL
          , ecmformula = NULL, decm = "Hurst"
          , drop = 5, report.data = TRUE, ...)
}
\arguments{
  \item{formula}{
  An object of the class ``\code{\link{formula}}'' that specifies the multilevel model to be estimated (see \code{\link{lmer}} for details): a two-sided linear formula object describing both the fixed-effects and fixed-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. Random-effects terms are distinguished by vertical bars (``|'') separating expressions for design matrices from grouping factors (i.e. the variable indicating the timepoints of the repeated cross-sectional design as well as potentially further clustering variables). See details below for further information about selecting variables for automatic aggregation, fractional differencing, and the removal of deterministic components.
}
  \item{data}{
  Data frame containing the original variables named in \code{formula}.
}
  \item{timevar}{
  Name of the variable indicating different timepoints in \code{data}.
}
  \item{d}{
Call for a specific estimation method for the fractional differencing parameter in the \code{\link[fractal:hurstSpec]{fractal}}-package (\code{``Hurst''}) or in the \code{\link[fracdiff:fracdiff]{fracdiff}}-package (\code{``ML''}, \code{``GPH''}, or \code{``Sperio''}). Default estimation procedure is by estimating the \code{Hurst} exponent. If the user wants to specify the methods for each variable individually, \code{d} can be a list containing a call for every individual variable. Furthermore, the list can contain numeric values for differencing parameters which were estimated externally (e.g. 1 for simple differencing, also see example for further details). A variable will not be differenced if \code{d} is specified as 0.
}
  \item{arma}{
List of variables for which AR and MA parameters are to be estimated (after fractional differencing) as well as a vector containing the respective orders of the model to fit. \code{order[1]} corresponds to the AR part and \code{order[2]} to the MA part, similar to the model specification in \code{\link[stats:arima]{arima}} (just excluding the d parameter here). For variables specified in \code{arma}, the function will use the residuals of the ARMA model for the subsequent model estimation in order to remove their deterministic components. All variables included in the \code{arma} list have to included in either \code{varlist.fd}, \code{varlist.ydif}, or \code{varlist.xdif}. It is also possible to keep some of the AR/MA parameters fixed at zero (e.g. if the model is only supposed to estimate AR[1] and AR[3] parameters, but not AR[2]). In order to specify such a model, replace the vector containing the orders of the model with a list containing two vectors indicating each individual AR or MA parameter to be estimated. Please see the examples for clarification.
}
  \item{ecmformula}{
Specification of the cointegration regression to receive the residuals for the error correction mechanism (ecm) included in \code{formula}: linear formula object with the response on the left of a ~ operator and the independent variables, separated by + operators, on the right. Note that the variable names included here cannot be the original variable names included in \code{data}, but rather has to be extended by adding ``\code{.mean}'' to the original names, since the ecm is always based on the level/time aggreegates (see example).
}
  \item{decm}{
Call for estimation method for the fractional differencing parameter (see \code{d} for details). Can be either ``\code{Hurst}'', ``\code{ML}'', ``\code{GPH}'', or ``\code{Sperio}''. Default is ``\code{Hurst}''.
}
  \item{drop}{
Number of time points from the beginning of the series dropped from analysis. Default is 5.
}
  \item{report.data}{
Logical. \code{arfimaMLM} returns the transformed dataset used to estimate the final model as part of the results. Default is \code{TRUE}.
}
  \item{\dots}{
Further arguments passed to the estimation procedures used within the function (e.g. for \code{lmer}).
}
}
\details{
\itemize{
\item The original variable names of \code{data} used in \code{formula} can be extended by adding three different suffixes: \code{.fd}, \code{.xdif}, and \code{.ydif}. These suffixes select variables for transformations according to the Arfima-MLM framework before estimating the actual model.

The suffix \code{.fd} allows the user to select variables which are supposed to be transformed to a fractionally differenced level-variable (by aggregating individuals over each time point prior to fractionally differencing the series), or variables which are already included as a level-variable in the original dataset and are just supposed to be fractionally differenced before the multilevel model is estimated. 

If the suffix \code{.xdif} is added to an independent variable, the variable is simply filtered through the timepoint averages:\deqn{x.star[it]=x[it]-X[t]}

If the suffix \code{.ydif} is added to the dependent variable (e.g. \eqn{y[it]}), the function will remove the temporal deterministic component from the individual level variable, such that it only consists of within-timepoint, as well as non-temporally autocorrelated between-timepoint variation:\deqn{y.star[it]=y[it]-(Y[t]-\Delta[df]Y[t])}

\item If \code{formula} contains \code{ecm} as one of the independent variables, and \code{ecmformula} is correctly specified, the function will include the lag of the fractionally differenced residuals of the cointegration regression as an error correction mechanism in the multilevel model. The ECM does not have to be estimated prior to calling the function.

\item In order to prevent errors in the estimation procedure, none of the original variable names in \code{data} should include ``\code{.ydif}'', ``\code{.xdif}'' or ``\code{.fd}''.
}
}
\value{
The function returns a list of the class 'arfimaMLM' with the following items:
\item{result}{Output of the multilevel model as specified in \code{formula}.}
\item{d}{Matrix of fractional differencing parameters estimated for the level variables (\code{.fd} and \code{.ydif}) as well as the estimation method for each variable. Returns the specified value for d if it was specified in the initial call of the function.}
\item{arma}{List of \code{arima} results for each variable specified in the model call. Contains AR/MA estimates as well as the model residuals.}
\item{ecm}{Output of the cointegration regression (returned if \code{ecmformula} is specified). The lagged residuals of the cointegration regression are included in the multilevel model if \code{ecm} is included in \code{formula}.}
\item{data.mean}{Data frame of variable means declared in \code{formula} as \code{.fd}, \code{.xdif} or \code{.ydif} (as well as \code{.mean} in \code{ecmformula}) for each time point specified by the level variable in \code{timevar}.}
\item{data.fd}{Data frame of fractionally differenced level variables declared in \code{formula} as \code{.fd} or \code{.ydif} for each time point specified by the level variable in \code{timevar}. If \code{arma} was additionally specified for a variable, it contains the residuals of the ARMA model fitted after (fractionally) differencing.}
\item{data.merged}{Merged data frame used to estimate the multilevel model consisting of the original data, \code{data.mean}, \code{data.fd}, as well as the variables specified in \code{formula} as \code{.xdif} and \code{.ydif}}
}
\references{
Lebo, M. and Weber, C. 2015. ``An Effective Approach to the Repeated Cross Sectional Design.'' American Journal of Political Science 59(1): 242-258.
}
\author{
Patrick Kraft
}
\note{
%%  ~~further notes~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[lme4:lme4]{lme4}}, \code{\link[fracdiff:fracdiff]{fracdiff}}, \code{\link[fractal:hurstSpec]{hurstSpec}}, \code{\link[ArfimaMLM:arfimaPrep]{arfimaPrep}}, \code{\link[ArfimaMLM:fd]{fd}}, and \code{\link[ArfimaMLM:ArfimaMLM]{ArfimaMLM}} for a package overview.
}
\examples{
require(fractal)
require(fracdiff)
require(lme4)

### set basic parameters for simulation
t = 100 # number of time points
n = 500 # number of observations within time point
N = t*n # total number of observations

### generate fractional ARIMA Time Series for y_t, x1_t, z1_t, z2_t
set.seed(123)
y_t <- fracdiff.sim(t, d=0.4, mu=10)$series
x1_t <- fracdiff.sim(t, d=0.3, mu=5)$series
z1_t <- fracdiff.sim(t, d=0.1, mu=2)$series
z2_t <- fracdiff.sim(t, d=0.25, mu=3)$series

### simulate data
data <- NULL; data$time <- rep(seq(1:t),each=n)
data <- data.frame(data)
data$x1 <- rnorm(N,rep(x1_t,each=n),2)
data$x2 <- rnorm(N,0,40)
data$z1 <- rnorm(N,rep(z1_t,each=n),3)
data$z2 <- rep(z2_t,each=n)
b1 <- 0.2+rep(rnorm(t,0,0.1),each=n)
data$y <- (b1*data$x1-0.05*data$x2+0.3*rep(z1_t,each=n)
            +0*data$z2+rnorm(N,rep(y_t,each=n),1))


### estimate models

# basic example
m1 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + (1 | time)
                , data = data, timevar = "time")

# model including error correction mechanism, change estimation method for differencing parameter for all variables
m2 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + ecm + (1 | time)
                , data = data, timevar = "time", d="ML"
                , ecmformula = y.mean ~ x1.mean, decm="Sperio")

# vary estimation method for differencing parameter between variables, specify AR/MA models
m3 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + (1+x1.xdif|time)
                , data = data, timevar = "time"
                , d=list(y="ML", z1="Sperio", z2=0.25)
                , arma=list(y=c(1,0),z2=c(0,1)))                

# specify AR/MA models while holding AR[2] fixed for y
m4 <- arfimaMLM(y.ydif ~ x1.xdif + x2 + z1.fd + z2.fd + (1+x1.xdif|time)
                , data = data, timevar = "time"
                , arma=list(y=list(c(1,3),0),z2=c(0,1)))                 
                
m1
summary(m2)
summary(m3$result)
m4$arma
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{manip}
\keyword{models}
\keyword{multivariate}