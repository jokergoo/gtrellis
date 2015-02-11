\name{add_track}
\alias{add_track}
\title{
add graphics by track  


}
\description{
add graphics by track  


}
\usage{
add_track(gr = NULL, category = NULL, i_track = get_cell_meta_data("i_track")+1,
    clip = TRUE, panel.fun = function(gr) NULL)
}
\arguments{

  \item{gr}{a data frame in BED format or a \code{GRanges} object.}
  \item{category}{categories (e.g. chromosome)}
  \item{i_track}{which track, by default it is the next track}
  \item{clip}{whether graphics are restricted inside the cell}
  \item{panel.fun}{panel function to add graphics on each 'cell'}

}
\details{
As same as \code{panel.fun} in \code{circlize} package, \code{panel.fun} will be applied on every cell in the current track. \code{gr} in \code{panel.fun} is a subset of the main \code{gr} which only contains data for the current category. 


}
