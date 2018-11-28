\name{add_lines_track}
\alias{add_lines_track}
\title{
add lines to a new or exsited track
}
\description{
add lines to a new or exsited track
}
\usage{
add_lines_track(gr, value, area = FALSE, baseline = "bottom", gp = gpar(), ...)
}
\arguments{

  \item{gr}{genomic regions, it can be a data frame or a \code{\link[GenomicRanges:GRanges-class]{GRanges}} object}
  \item{value}{numeric values associated with \code{gr}}
  \item{area}{whether draw polygon for the area under the line}
  \item{baseline}{baseline for drawing polygon}
  \item{gp}{graphic settings, should be specified by \code{\link[grid]{gpar}}.}
  \item{...}{other arguments passed to \code{\link{add_track}}}

}
\value{
No value is returned.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
require(circlize)
bed = generateRandomBed(200)
gtrellis_layout(n_track = 2, track_ylim = rep(range(bed[[4]]), 2), nrow = 3, byrow = FALSE)
add_lines_track(bed, bed[[4]])
add_lines_track(bed, bed[[4]], area = TRUE, gp = gpar(fill = "grey", col = NA))
}
