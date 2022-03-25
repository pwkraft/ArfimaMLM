\name{arfimaPrep}
\alias{arfimaPrep}
\title{
Preparing a dataset for subsequent analysis accoring to the Arfima-MLM/Arfima-OLS framework
}
\description{
This function prepares a repeated cross-sectional dataset or pooled cross-sectional time-series data for subsequent analyses according to the Arfima-MLM/Arfima-OLS framework. This function is mainly used internally as part of \code{\link{arfimaMLM}} and \code{\link{arfimaOLS}}, but can also be used independently if the user prefers to separate the data preparation from the subsequent estimation of the multilevel (or simple linear) model. The function performs the aggregation and fractional differencing of time/level variables as well as the necessary procedures to remove deterministic components from the dependent as well as the major independent variables.
}
\usage{
arfimaPrep(data, timevar
           , varlist.mean, varlist.fd
           , varlist.xdif, varlist.ydif
           , d = "ML", arma = NULL
           , ecmformula = NULL, decm = "ML"
           , drop = 5, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Data frame to be transformed by the function.
}
  \item{timevar}{
  Name of the variable indicating different timepoints in \code{data}.
}
  \item{varlist.mean}{
  Character vector of variable names in \code{data} that are averaged/aggregated over each timepoint specified by \code{timevar}. The variable list must include all variables listed in \code{varlist.fd}, \code{varlist.xdif}, \code{varlist.ydif}
}
  \item{varlist.fd}{
  Character vector of variable names in \code{data} that are fractionally differenced (after aggregating over each timepoint specified by \code{timevar}). The variable list must include all variables listed in \code{varlist.ydif} See details for further information.
}
  \item{varlist.xdif}{
   Character vector of variable names in \code{data} for which the within-timepoint deviation from the respective mean value is calculated (for each timepoint specified by \code{timevar}). See details for further information.
}
  \item{varlist.ydif}{
  Character vector of variable names in \code{data} for which the temporal deterministic component is removed by substracting the difference of the within-timepoint average and its stationary series free of autocorrelation (with each timepoint specified by \code{timevar}). See details for further information.
}
  \item{d}{
Call for a specific estimation method for the fractional differencing parameter in the \code{\link[fractal:hurstSpec]{fractal}}-package (\code{``Hurst''}) or in the \code{\link[fracdiff:fracdiff]{fracdiff}}-package (\code{``ML''}, \code{``GPH''}, or \code{``Sperio''}). Default estimation procedure is by estimating the \code{ML} exponent. If the user wants to specify the methods for each variable individually, \code{d} can be a list containing a call for every individual variable. Furthermore, the list can contain numeric values for differencing parameters which were estimated externally (e.g. 1 for simple differencing, also see example for further details). A variable will not be differenced if \code{d} is specified as 0.
}
  \item{arma}{
List of variables for which AR and MA parameters are to be estimated (after fractional differencing) as well as a vector containing the respective orders of the model to fit. \code{order[1]} corresponds to the AR part and \code{order[2]} to the MA part, similar to the model specification in \code{\link[stats:arima]{arima}} (just excluding the d parameter here). For variables specified in \code{arma}, the function will use the residuals of the ARMA model for the subsequent model estimation in order to remove their deterministic components. All variables included in the \code{arma} list have to included in either \code{varlist.fd}, \code{varlist.ydif}, or \code{varlist.xdif}. It is also possible to keep some of the AR/MA parameters fixed at zero (e.g. if the model is only supposed to estimate AR[1] and AR[3] parameters, but not AR[2]). In order to specify such a model, replace the vector containing the orders of the model with a list containing two vectors indicating each individual AR or MA parameter to be estimated. Please see the examples for clarification.
}
  \item{ecmformula}{
Specification of the cointegration regression to receive the residuals for the error correction mechanism (ecm) to be included in the transfromed dataset: linear formula object with the response on the left of a ~ operator and the independent variables, separated by + operators, on the right. Note that the variable names included here cannot be the original variable names included in \code{data}, but rather has to be extended by adding ``\code{.mean}'' to the original names, since the ecm is always based on the level/time aggreegates (see example).
}
  \item{decm}{
Call for estimation method for the fractional differencing parameter (see \code{d} for details). Can be either \code{``Hurst''} \code{``ML''}, \code{``GPH''}, or \code{``Sperio''}. Default is \code{``ML''}.
}
  \item{drop}{
Number of time points from the beginning of the series dropped from analysis. Default is 5.
}
  \item{\dots}{
Further arguments passed to the estimation procedures used within the function.
}
}
\details{
\itemize{
\item The varlists \code{varlist.fd}, \code{varlist.xdif}, and \code{varlist.ydif} select variables from \code{data} for transformations according to the Arfima-MLM framework to prepare the estimation of the actual model.

Adding variables in \code{varlist.fd} allows the user to select variables which are supposed to be transformed to a fractionally differenced level-variable (by aggregating individuals over each time point prior to fractionally differencing the series), or variables which are already included as a level-variable in the original dataset and are just supposed to be fractionally differenced before the multilevel model is estimated. 

For variables in \code{varlist.xdif}, the corresponding variables in \code{data} is simply filtered through the timepoint averages:\deqn{x.star[it]=x[it]-X[t]}

For variables in \code{varlist.ydif} (e.g. \eqn{y[it]}), the function will remove the daily deterministic component from the individual level variable, such that it only consists of within-timepoint, as well as non-temporally autocorrelated between-timepoint variation: \deqn{y.star[it]=y[it]-(Y[t]-\Delta[df]Y[t])}

\item In order to prevent errors in the estimation procedure, none of the original variable names in \code{data} should include ``.fd'', ``.xdif'', or ``.ydif''.
}
}
\value{
The function returns a list of datasets and estimation results with the following items:
\item{data.mean}{Data frame of variable means declared in \code{varlist.mean}, \code{varlist.fd}, \code{varlist.xdif}, or \code{varlist.ydif} for each time point specified by the level variable in \code{timevar}.}
\item{data.fd}{Data frame of fractionally differenced level variables for each time point specified in \code{timevar}, which were declared as \code{.fd} or \code{.ydif} in \code{formula}. If \code{arma} was additionally specified for a variable, it contains the residuals of the ARMA model fitted after (fractionally) differencing.}
\item{data.merged}{Merged data frame which can be subsequently used to estimate the multilevel model. Consitst of the original data, \code{data.mean}, \code{data.fd}, as well as the variables specified in \code{varlist.xdif} and \code{.ydif}}
\item{d}{Matrix of fractional differencing parameters estimated for the level variables (\code{varlist.fd} and \code{varlist.ydif}) as well as the estimation method for each variable. Returns the specified value for d if it was specified in the initial call of the function.}
\item{arma}{List of \code{arima} results for each variable specified in the model call. Contains AR/MA estimates as well as the model residuals.}
\item{ecm}{Output of the cointegration regression (returned if \code{ecmformula} is specified). The lagged residuals of the cointegration regression are included in \code{data.fd} and \code{data.merged}.}
}
\references{
Lebo, M. and Weber, C. 2015. ``An Effective Approach to the Repeated Cross Sectional Design.'' American Journal of Political Science 59(1): 242-258.
}
\author{
Patrick Kraft
}
%\note{
%%  ~~further notes~~
%}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[fracdiff:fracdiff]{fracdiff}}, \code{\link[fractal:hurstSpec]{hurstSpec}}, \code{\link[ArfimaMLM:fd]{fd}}, and \code{\link[ArfimaMLM:ArfimaMLM]{ArfimaMLM}} for a package overview.
}
\examples{
require(fracdiff)

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


### prepare datasets for model estimation

# basic example
dat1 <- arfimaPrep(data = data, timevar="time"
                   , varlist.mean = c("y","x1","z1","z2")
                   , varlist.fd = c("y", "z1","z2")
                   , varlist.xdif = "x1", varlist.ydif = "y")
                   
# including error correction mechanism
# change estimation method for differencing parameter for all variables
dat2 <- arfimaPrep(data = data, timevar="time"
                   , varlist.mean = c("y","x1","z1","z2")
                   , varlist.fd = c("y", "z1","z2")
                   , varlist.xdif = "x1", varlist.ydif = "y"
                   , d = "ML", ecmformula = y.mean ~ x1.mean
                   , decm="Sperio")
                   
# vary estimation method for differencing parameter between variables
# specify AR/MA models                   
dat3 <- arfimaPrep(data = data, timevar="time"
                   , varlist.mean = c("y","x1","z1","z2")
                   , varlist.fd = c("y", "z1","z2")
                   , varlist.xdif = "x1", varlist.ydif = "y"
                   , d=list(y="ML", z1="Sperio", z2=0.25)
                   , arma=list(y=c(1,0),z2=c(0,1)))

# specify AR/MA models while holding AR[2] fixed for y
dat4 <- arfimaPrep(data = data, timevar="time"
                   , varlist.mean = c("y","x1","z1","z2")
                   , varlist.fd = c("y", "z1","z2")
                   , varlist.xdif = "x1", varlist.ydif = "y"
                   , arma=list(y=list(c(1,3),0),z2=c(0,1)))                   

ls(dat1)
head(dat1$mean)
head(dat2$merged)
dat3$arma
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{manip}