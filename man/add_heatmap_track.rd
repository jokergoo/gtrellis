\name{add_heatmap_track}
\alias{add_heatmap_track}
\title{
add heatmap to a new track
}
\description{
add heatmap to a new track
}
\usage{
add_heatmap_track(gr, mat, fill, border = NA, track = get_cell_meta_data("track") + 1, ...)
}
\arguments{

  \item{gr}{genomic regions, it can be a data frame or a \code{\link[GenomicRanges]{GRanges}} object}
  \item{mat}{matrix in which rows correspond to intervals in \code{gr}}
  \item{fill}{a color mapping function which maps values to colors. Users can consider \code{\link[circlize]{colorRamp2}} to generate a color mapping function.}
  \item{border}{border of the grids in heatmap}
  \item{track}{which track the graphics will be added to. By default it is the next track. The value should only be a scalar.}
  \item{...}{other arguments passed to \code{\link{add_track}}}

}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\seealso{
\code{\link{add_rect_track}}, \code{\link{add_track}}
}
\examples{
require(circlize)
bed = generateRandomBed(200)
col_fun = colorRamp2(c(-1, 0, 1), c("green", "black", "red"))
gtrellis_layout(nrow = 3, byrow = FALSE, track_axis = FALSE)
mat = matrix(rnorm(nrow(bed)*4), ncol = 4)
add_heatmap_track(bed, mat, fill = col_fun)
}
