\name{add_track}
\alias{add_track}
\title{
Add self-defined graphics track by track
}
\description{
Add self-defined graphics track by track
}
\usage{
add_track(gr = NULL, category = NULL, track = get_cell_meta_data("track") + 1,
    clip = TRUE, panel.fun = function(gr) NULL)
}
\arguments{

  \item{gr}{genomic regions. It should be a data frame in BED format or a \code{GRanges} object.}
  \item{category}{subset of categories (e.g. chromosomes) that users want to add graphics. The value can be a vector which contains more than one category. By default itis all available categories.}
  \item{track}{which track the graphics will be added to. By default it is the next track. The value should only be a scalar.}
  \item{clip}{whether graphics are restricted inside the cell.}
  \item{panel.fun}{self-defined panel function to add graphics in each 'cell'. THe argument \code{gr} in \code{panel.fun} only contains data for the current category which is a subset of the main \code{gr}.}

}
\details{
Initialization of the Trellis layout and adding graphics are two independent steps.
Once the layout initialization finished, each cell will be an independent plotting region.
As same as \code{panel.fun} in \code{\link[circlize]{circlize-package}}, the self-defined function \code{panel.fun}
will be applied on every cell in the specified track (by default it is the 'current' track).

When adding graphics in each cell, \code{\link{get_cell_meta_data}} can return several meta data for the current cell.

Since this package is implemented by the \code{grid} graphic system, \code{grid}-family functions
(such as \code{\link[grid]{grid.points}}, \code{\link[grid]{grid.rect}}, ...) should be used to add graphics. The usage
of \code{grid} functions is quite similar as the traditional graphic functions. 
Followings are several examples:

  \preformatted{
    grid.points(x, y)
    grid.lines(x, y)
    grid.rect(x, y, width, height)  }

Graphical parameters are usually passed by \code{\link[grid]{gpar}}:

  \preformatted{
    grid.points(x, y, gp = gpar(col = "red")
    grid.rect(x, y, width, height, gp = gpar(fill = "black", col = "red"))  }

\code{grid} system also support a large number of coordinate measurement systems by defining proper \code{\link[grid]{unit}} object 
which provides high flexibility to place graphics on the plotting regions.

  \preformatted{
    grid.points(x, y, default.units = "npc")
    grid.rect(x, y, width = unit(1, "cm"))  }

You can refer to the documentations and vignettes of \code{\link[grid]{grid-package}} to get a overview.
}
\value{
No value is returned.
}
\seealso{
\code{\link{get_cell_meta_data}}
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
require(circlize)
bed = circlize::generateRandomBed()
gtrellis_layout(track_ylim = range(bed[[4]]))
add_track(bed, panel.fun = function(bed) {
    x = (bed[[2]] + bed[[3]]) / 2
    y = bed[[4]]
    grid.points(x, y, pch = 16, size = unit(0.5, "mm"))
})

# you can add graphics in any cell by specifying `category` and `track`
all_chr = paste0("chr", 1:22)
letter = strsplit("MERRY CHRISTMAS!", "")[[1]]
gtrellis_layout(nrow = 5)
for(i in seq_along(letter)) {
    add_track(category = all_chr[i], track = 1, panel.fun = function(gr) {
        grid.text(letter[i], gp = gpar(fontsize = 30))
    })
}

}
