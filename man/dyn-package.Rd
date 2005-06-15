\name{dyn-package}
\alias{dyn-package}
\docType{package}
\title{
Time Series Regression
}
\description{
Time series regression.  The dyn class interfaces ts,
irts, its, zoo and zooreg time series classes to lm, glm,
loess, quantreg::rq, MASS::rlm, quantreg::rq,
randomForest::randomForest and other regression functions
allowing those functions to be used with time series including
specifications that may contain lags, diffs and missing
values.  
}
\details{
\code{"dyn"} allows one to use time series with regression functions
that were not originally written to support time series by simply
prefacing the call to the regression function with \code{"dyn$"}.  The 
following are sources of information on \code{"dyn"}:
\tabular{ll}{
Overview\tab file.show(system.file("README", package = "dyn"))\cr
News\tab file.show(system.file("NEWS", package = "dyn"))\cr
Acknowledgements\tab file.show(system.file("THANKS", package = "dyn"))\cr
Wish List\tab file.show(system.file("WISHLIST", package = "dyn"))\cr
List of demo files\tab demo(package = "dyn"))\cr
Invoking a demo file\tab demo("dyn-rq")\cr
This File\tab package?dyn\cr
Help file\tab ?dyn\cr
}
}

\keyword{models}
\examples{
x <- ts(seq(10)^2)
dyn$lm(x ~ lag(x,-1))
dyn$glm(x ~ lag(x,-1))
dyn$loess(x ~ lag(x,-1))
}
