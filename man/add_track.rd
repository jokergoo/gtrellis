\name{add_track}
\alias{add_track}
\title{
add graphics by track  


}
\description{
add graphics by track  


}
\usage{
add_track(gr = NULL, category = NULL, i_track = get_cell_meta_data("i_track") + 1,
    clip = TRUE, panel.fun = function(gr) NULL)
}
\arguments{

  \item{gr}{genomic regions. It should be a data frame in BED format or a \code{\link[GenomicRanges]{GRanges}} object.}
  \item{category}{subset of categories (e.g. chromosomes)}
  \item{i_track}{which track the graphics will be added to. By default it is the next track}
  \item{clip}{whether graphics are restricted inside the cell}
  \item{panel.fun}{self-defined panel function to add graphics in each 'cell'}

}
\details{
Initialization of the Trellis layout and adding graphics are two independent steps. Once the initialization finished, each cell or panel is an independent plotting region. As same as \code{panel.fun} in \code{circlize} package, the self-defined function \code{panel.fun} will be applied on every cell in the specified track (by default it is the 'current' track).   

\code{gr} in \code{panel.fun} is a subset of the main \code{gr} which only contains data for the current category.  

Note \code{category} can be a vector with length larger than 2 while \code{i_track} can only be a scalar.  


}
\value{
no value returned  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
require(circlize)
bed = circlize::generateRandomBed()
initialize_layout(track_ylim = range(bed[[4]]))
add_track(bed, panel.fun = function(bed) {
    x = (bed[[2]] + bed[[3]]) / 2
    y = bed[[4]]
    grid.points(x, y, pch = 16, size = unit(0.5, "mm"))
})

# you can add graphics in any cell by specifying `category` and `i_track`
all_chr = paste0("chr", 1:22)
letter = strsplit("MERRY CHRISTMAS!", "")[[1]]
initialize_layout(nrow = 5)
for(i in seq_along(letter)) {
    add_track(category = all_chr[i], i_track = 1, panel.fun = function(gr) {
        grid.text(letter[i], gp = gpar(fontsize = 30))
    })
}
}
