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
  \item{name}{name of the category.}
  \item{xlim}{xlim without including padding. Cells in the same column share the same \code{xlim}.}
  \item{ylim}{ylim without including padding.}
  \item{extended_xlim}{xlim with padding.}
  \item{extended_ylim}{ylim with padding.}
  \item{original_xlim}{xlim in original data.}
  \item{original_ylim}{ylim in original data.}
  \item{column}{which column in the layout.}
  \item{row}{which row in the layout.}
  \item{track}{which track in the layout.}
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
gtrellis_layout(ncol = 4, n_track = 3)
add_track(panel.fun = function(gr) {
    print(get_cell_meta_data("xlim"))
    print(get_cell_meta_data("ylim"))
    print(get_cell_meta_data("extended_xlim"))
    print(get_cell_meta_data("extended_ylim"))
    print(get_cell_meta_data("original_xlim"))
    print(get_cell_meta_data("original_ylim"))
    print(get_cell_meta_data("name"))
    print(get_cell_meta_data("column"))
    print(get_cell_meta_data("row"))
    print(get_cell_meta_data("track"))
    cat("\n\n")
})

for(chr in paste0("chr", 1:22)) {
    print(get_cell_meta_data("xlim", category = chr, track = 1))
    print(get_cell_meta_data("ylim", category = chr, track = 1))
    print(get_cell_meta_data("extended_xlim", category = chr, track = 1))
    print(get_cell_meta_data("extended_ylim", category = chr, track = 1))
    print(get_cell_meta_data("original_xlim", category = chr, track = 1))
    print(get_cell_meta_data("original_ylim", category = chr, track = 1))
    print(get_cell_meta_data("name", category = chr, track = 1))
    print(get_cell_meta_data("column", category = chr, track = 1))
    print(get_cell_meta_data("row", category = chr, track = 1))
    print(get_cell_meta_data("track", category = chr, track = 1))
    cat("\n\n")
}

}
