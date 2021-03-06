% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-stats.R
\name{doSampleVariance}
\alias{doSampleVariance}
\title{doSampleVariance}
\usage{
doSampleVariance(x, method = "two-pass")
}
\arguments{
\item{x}{numeric vector}

\item{method}{"two-pass" prevents "naive" floating-point issues}
}
\value{
list (x.bar, s.var, s.sd)
}
\description{
Computes the sample variance with (n-1) ...
}
\examples{
doSampleVariance( c(1) ); # returns null
doSampleVariance( 1:2 );

}
\seealso{
Other Stats: 
\code{\link{calculateZscores}()},
\code{\link{doModeOpposite}()},
\code{\link{doMode}()},
\code{\link{doStatsSummary}()},
\code{\link{findOutliersUsingIQR}()},
\code{\link{findOutliersUsingZscores}()}
}
\concept{Stats}
