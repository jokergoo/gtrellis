\name{add_segments_track}
\alias{add_segments_track}
\title{
add segments to a new or exsited track
}
\description{
add segments to a new or exsited track
}
\usage{
add_segments_track(gr, value, gp = gpar(), ...)
}
\arguments{

  \item{gr}{genomic regions, it can be a data frame or a \code{\link[GenomicRanges]{GRanges}} object}
  \item{value}{numeric values associated with \code{gr}}
  \item{gp}{graphic settings, should be specified by \code{\link[grid]{gpar}}.}
  \item{...}{other arguments passed to \code{\link{add_track}}}

}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
require(circlize)
bed = generateRandomBed(nr = 100)
gtrellis_layout(track_ylim = range(bed[[4]]), nrow = 3, byrow = FALSE)
add_segments_track(bed, bed[[4]], gp = gpar(col = ifelse(bed[[4]] > 0, "red", "green"), lwd = 4))
}
