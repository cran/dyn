\name{dyn}
\alias{dyn}
\alias{$.dyn}
\alias{model.frame.dyn}
\alias{model.frame.irts}
\alias{model.frame.its}
\alias{model.frame.ts}
\alias{model.frame.zoo}
\alias{model.frame.zooreg}
\alias{predict.dyn}
\alias{predict.ts}
\alias{predict.zoo}
\alias{fitted.dyn}
\alias{fitted.ts}
\alias{fitted.zoo}
\alias{residuals.dyn}
\alias{residuals.ts}
\alias{residuals.zoo}
\alias{update.dyn}
\alias{anova.dyn}
\alias{fitted.irts}
\alias{fitted.its}
\alias{fitted.zooreg}
\alias{predict.irts}
\alias{predict.its}
\alias{predict.zooreg}
\alias{residuals.irts}
\alias{residuals.its}
\alias{residuals.zooreg}
\concept{dynamic regression}
\title{ dynamic regression class }
\description{
dyn is constructs objects of class \code{"dyn"},
}
\usage{
dyn(x)
}
\arguments{
  \item{x}{ an object, typically a \code{"formula"} object or
  an object produced by \code{"lm"}, \code{"glm"} or other
  regression function.}
}
\details{

  \code{"dyn"} enables regression functions that were not written to handle
  time series to so handle them.  The time series need not have the same
  indexes (the are automatically intersected) and may have missing
  values including internal missing values. 

  \code{"dyn"} creates a dynamic regression object by
  returning \code{x} with the \code{"dyn"} class name 
  prepended to the class of the argument.  

  If the argument to \code{"dyn"}
  is a formula its variables may be time series objects of one
  of the following classes: \code{"ts"}, \code{"irts"},
  code{"its"}, \code{"zoo"} or \code{"zooreg"}.  

  \code{"dyn"} methods are available for \code{"model.frame"}, \code{"fitted"},
  \code{"residuals"}, \code{"predict"}, \code{"update"}, \code{"anova"}
  and \code{"$"}.
  These methods preprocess their arguments, 
  call the real method which does the actual work
  and then post process the returned object.  In the case of \code{"fitted"},
  \code{"residuals"} and \code{"predict"} they ensure that the result is
  a time series.  In the case of \code{anova} the objects are
  intersected so that they all have the time indexes
  to ensure that a meaningful input is provided to \code{"anova"}.

  The \code{$} method is always used with a left argument of \code{"dyn"}
  like this \code{"dyn$lm(x, ...)"}.  This expression  is equivalent to 
  \code{"dyn(lm(dyn(x), ...))"} but is more convenient to write.

  \code{"dyn"} currently works with any regression function that makes
  use of \code{"model.frame"} and is written in the style of \code{"lm"}.
  This includes \code{"lm"}, \code{"glm"}, \code{"loess"}, \code{"rlm"}
  (from \pkg{"MASS"}), \code{"lqs"} (from pkg{"MASS"}), \code{"randomForest"}
  (from \pkg{"randomForest"}), \code{"rq"} (from \pkg{"quantreg"}) and
  likely others.

}
\value{
  \code{"dyn"} returns its argument with the class name \code{"dyn"}
  prepended to its class vector.  The \code{"fitted"}, \code{"residuals"}
  and \code{"predict"} \code{"dyn"} methods return time series of the
  appropriate class.  \code{"model.frame"} creates a model frame with
  an attribute of \code{"series"} that contains a data frame of 
  the time series and factor variables as columns.
}
\note{\code{"dyn"} relies on the underlying time series classes
  and regression routines for all substantive functionality.
  In particular note these limitations: \code{"irts"} has no
  \code{"lag"} or \code{"diff"} methods.  The lag function of
  \code{"its"} its called \code{"lagIts"}.  \code{"ts"} and
  \code{"zooreg"} series can be lagged outside of the data
  range (both forward and backward) but other time series
  classes cannot represent such data and therefore will drop them.
  If the regression function in questions does not have an
  associated \code{"fitted"}, \code{"residuals"}, etc. method
  then such method will not be available with \code{"dyn"} either.  

  Internally the system uses \code{"zoo"}.  Additional time series 
  classes not already defined to work with \code{"dyn"}
  can be added by simply defining \code{"as"} methods between
  the new class and \code{"zoo"} and then creating new methods (for 
  \code{"model.frame"}, \code{"predict"}, \code{"fitted"}, etc.)
  In most cases these method names can be set equal to the
  corresponding \code{"zoo"} method name (e.g. 
  \code{"model.frame.newclass <- model.frame.zoo"} so that
  no new function bodies need be written).

  The main requirements for new regression routines to work with
  \code{"dyn"} are that they use \code{"model.frame"}, that their
  \code{"fitted"}, \code{"residuals"} and \code{"predict"} methods
  return named vectors whose names are the corresponding indexes
  in the original data and that they follow the same style of
  processing as \code{"lm"}.  There is no \code{"dyn"} code
  specific to any particular regression routine.
}

\seealso{ See Also
\code{\link{model.frame}}, 
\code{\link{predict}}, 
\code{\link{fitted}}, 
\code{\link{residuals}}, 
\code{\link{anova}}, 
\code{\link{update}}, 
\code{\link{lm}}, 
\code{\link{glm}}, 
\code{\link{loess}} 
}
\examples{
y <- ts(1:12, start = c(2000,2), freq = 4)^3
x <- ts(1:9, start = c(2000,3), freq = 4)^2

# can be used with numerous different regression functions
y.lm <- dyn$lm( window(y, start = c(2000,4)) ~ diff(x) )
y.lm <- dyn$lm( y ~ diff(x) )
y.glm <- dyn$glm( y ~ diff(x) )
y.loess <- dyn$loess( y ~ diff(x) )

y.lm <- dyn(lm(dyn(y ~ diff(x))))  # same
y.lm
summary(y.lm)
residuals(y.lm)
fitted(y.lm)
y2.lm <- update(y.lm, . ~ . + lag(x))
y2.lm
anova(y.lm, y2.lm)

# examples of using data
dyn$lm(y ~ diff(x), list(y = y, x = x))
dyn$lm(y ~ diffx, list(y = y, diffx = diff(x)))

# invoke model.frame on formula as a dyn object
dyn$model.frame( y ~ diff(x) )

}
\keyword{regression}
