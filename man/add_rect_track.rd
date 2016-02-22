\name{add_rect_track}
\alias{add_rect_track}
\title{
add retangles to a new or exsited track
}
\description{
add retangles to a new or exsited track
}
\usage{
add_rect_track(gr, h1, h2, gp = gpar(), ...)
}
\arguments{

  \item{gr}{genomic regions, it can be a data frame or a \code{\link[GenomicRanges]{GRanges}} object}
  \item{h1}{top/bottom positions for rectangles}
  \item{h2}{top/bottom positions for rectangles}
  \item{gp}{graphic settings, should be specified by \code{\link[grid]{gpar}}.}
  \item{...}{other arguments passed to \code{\link{add_track}}}

}
\value{
No value is returned.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\seealso{
\code{\link{add_heatmap_track}}, \code{\link{add_track}}
}
\examples{
require(circlize)
bed = generateRandomBed(200)
col_fun = colorRamp2(c(-1, 0, 1), c("green", "black", "red"))
gtrellis_layout(track_ylim = range(bed[[4]]), nrow = 3, byrow = FALSE)
add_rect_track(bed, h1 = bed[[4]], h2 = 0, 
    gp = gpar(col = NA, fill = col_fun(bed[[4]])))
}
