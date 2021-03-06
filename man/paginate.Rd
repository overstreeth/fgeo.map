% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tag_status_by_subquadrat.R
\name{paginate}
\alias{paginate}
\title{Label the four divisions of a quadrat -- each with four subquadrats.}
\usage{
paginate(x, bl = 1, br = 2, tr = 3, tl = 4,
  subquad_offset = NULL)
}
\arguments{
\item{x}{A dataframe with the variable \code{subquadrat}.}

\item{bl, br, tr, tl}{Number or character giving the label of the four
subquadrats on each or the four divisions of a quadrat: bottom left (bl),
bottom right (br), top right (tr), and top left (tl).}

\item{subquad_offset}{\code{NULL} or \code{-1}. This argument is for internal use. It
helps paginate to use the same code for data with two different types of
labels for the subquadrat variable: (1) that with subquadrat-columns
starting at 1; and (2) that with subquadrat-columns starting at 0.\preformatted{The four divisions of a quadrat (bl, br, tr, tl), each with four subquadrats:

tl       tr
----     -----
14 24    34 44
13 23    33 43

bl       br
----     -----
12 22    32 42
11 21    31 41
}}
}
\value{
A modified version of the input with the additional variable \code{page}.
}
\description{
This function makes it possible for \code{\link[=plot_tag_status_by_subquadrat]{plot_tag_status_by_subquadrat()}} to plot each individual map.
Each map corresponds to one page and includes four subquadrats. There are a
total of four maps per quadrat (i.e. a total of 16 subquadrats per quadrat).
}
\examples{
\dontrun{
library(dplyr)
viewfulltable <- tribble(
   ~QX,  ~QY,
  17.9,    0,
   4.1,   15,
   6.1, 17.3
)
with_subquad <- fgeo.tool::add_subquad(viewfulltable, 20, 20, 5, 5)

# Warning: Internal function
fgeo.map:::paginate(with_subquad)
fgeo.map:::paginate(with_subquad, "a", "b", "c", "d")
}
}
\seealso{
\code{\link[fgeo.tool:add_subquad]{fgeo.tool::add_subquad()}}.
}
\keyword{internal}
