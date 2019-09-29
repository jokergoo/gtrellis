\name{get_cell_meta_data}
\alias{get_cell_meta_data}
\title{
Get meta data in a cell
}
\description{
Get meta data in a cell
}
\usage{
get_cell_meta_data(name, category, track)
}
\arguments{

  \item{name}{name of the supported meta data, see 'details' section.}
  \item{category}{which category. By default it is the current category.}
  \item{track}{which track. By default it is the current track.}

}
\details{
Following meta data can be retrieved:

\describe{
  \item{\code{name}}{name of the category.}
  \item{\code{xlim}}{xlim without including padding. Cells in the same column share the same \code{xlim}.}
  \item{\code{ylim}}{ylim without including padding.}
  \item{\code{extended_xlim}}{xlim with padding.}
  \item{\code{extended_ylim}}{ylim with padding.}
  \item{\code{original_xlim}}{xlim in original data.}
  \item{\code{original_ylim}}{ylim in original data.}
  \item{\code{column}}{which column in the layout.}
  \item{\code{row}}{which row in the layout.}
  \item{\code{track}}{which track in the layout.}
}

The vignette has a graphical explanation of all these meta data.
}
\value{
Corresponding meta data that user queried.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL
}
