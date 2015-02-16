\name{get_cell_meta_data}
\alias{get_cell_meta_data}
\title{
get meta data in cell  


}
\description{
get meta data in cell  


}
\usage{
get_cell_meta_data(name, i_category, i_track)
}
\arguments{

  \item{name}{name, see 'details' section}
  \item{i_category}{which category. By default it is the current category}
  \item{i_track}{which track. By default it is the current track}

}
\details{
Following meta data can be retrieved:  

\describe{
  \item{name}{name}
  \item{xlim}{xlim without including padding, cells in a same column shares the same \code{xlim}}
  \item{ylim}{yiim without including padding}
  \item{extended_xlim}{xlim with padding}
  \item{extended_ylim}{ylim with padding}
  \item{original_xlim}{xlim in original data}
  \item{original_ylim}{ylim in original data}
  \item{i_col}{which column in the layout}
  \item{i_row}{which row in the layout}
  \item{i_track}{which track in the layout}
}

The vignette has a graphical explanation of all these meta data.  


}
\value{
Corresponding meta data thar user queried.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
initialize_layout(ncol = 4, n_track = 3)
add_track(panel.fun = function(gr) {
    print(get_cell_meta_data("xlim"))
    print(get_cell_meta_data("ylim"))
    print(get_cell_meta_data("extended_xlim"))
    print(get_cell_meta_data("extended_ylim"))
    print(get_cell_meta_data("original_xlim"))
    print(get_cell_meta_data("original_ylim"))
    print(get_cell_meta_data("name"))
    print(get_cell_meta_data("i_col"))
    print(get_cell_meta_data("i_row"))
    print(get_cell_meta_data("i_track"))
    cat("\n\n")
})

for(chr in paste0("chr", 1:22)) {
    print(get_cell_meta_data("xlim", i_category = chr, i_track = 1))
    print(get_cell_meta_data("ylim", i_category = chr, i_track = 1))
    print(get_cell_meta_data("extended_xlim", i_category = chr, i_track = 1))
    print(get_cell_meta_data("extended_ylim", i_category = chr, i_track = 1))
    print(get_cell_meta_data("original_xlim", i_category = chr, i_track = 1))
    print(get_cell_meta_data("original_ylim", i_category = chr, i_track = 1))
    print(get_cell_meta_data("name", i_category = chr, i_track = 1))
    print(get_cell_meta_data("i_col", i_category = chr, i_track = 1))
    print(get_cell_meta_data("i_row", i_category = chr, i_track = 1))
    print(get_cell_meta_data("i_track", i_category = chr, i_track = 1))
    cat("\n\n")
}
}
