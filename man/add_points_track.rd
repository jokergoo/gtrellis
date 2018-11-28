\name{add_points_track}
\alias{add_points_track}
\title{
add points to a new or exsited track
}
\description{
add points to a new or exsited track
}
\usage{
add_points_track(gr, value, pch = 16, size = unit(1, "mm"), gp = gpar(), ...)
}
\arguments{

  \item{gr}{genomic regions, it can be a data frame or a \code{\link[GenomicRanges:GRanges-class]{GRanges}} object}
  \item{value}{numeric values associated with \code{gr}}
  \item{pch}{shape of points}
  \item{size}{size of points, should be a \code{\link[grid]{unit}} object}
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
bed = generateRandomBed()
gtrellis_layout(track_ylim = range(bed[[4]]), nrow = 3, byrow = FALSE)
add_points_track(bed, bed[[4]], gp = gpar(col = ifelse(bed[[4]] > 0, "red", "green")))
}
