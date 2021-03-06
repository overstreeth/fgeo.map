% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sp_elev.R
\name{elev}
\alias{elev}
\title{Enable autoplotting the variable \code{elev}.}
\usage{
elev(elev)
}
\arguments{
\item{elev}{A ForestGEO-like elevation list or its \code{col} dataframe -- with
the variable \code{elev}.}
}
\value{
An S3 object of class 'elev'.
}
\description{
Enable autoplotting the variable \code{elev}.
}
\examples{
class(elev(fgeo.data::luquillo_elevation))
# Same
class(elev(fgeo.data::luquillo_elevation$col))
}
\seealso{
\code{\link[=autoplot.elev]{autoplot.elev()}}.

Other functions to construct fgeo classes: \code{\link{sp_elev}},
  \code{\link{sp}}

Other functions to plot elevation: \code{\link{autoplot.sp_elev}},
  \code{\link{autoplot_by_species.sp_elev}},
  \code{\link{sp_elev}}
}
\concept{functions to construct fgeo classes}
\concept{functions to plot elevation}
