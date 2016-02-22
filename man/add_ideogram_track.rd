\name{add_ideogram_track}
\alias{add_ideogram_track}
\title{
Add ideogram track
}
\description{
Add ideogram track
}
\usage{
add_ideogram_track(cytoband = paste0(system.file(package = "circlize"),
    "/extdata/cytoBand.txt"), species = NULL, track = current_track() + 1)
}
\arguments{

  \item{cytoband}{Path of the cytoband file or a data frame that already contains cytoband data. Pass to \code{\link[circlize]{read.cytoband}}.}
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download \code{cytoBand.txt.gz} from UCSC ftp automatically. Pass to \code{\link[circlize]{read.cytoband}}.}
  \item{track}{which track the ideogram is added in. By default it is the next track in the layout.}

}
\details{
A track which contains ideograms will be added to the plot.

The function tries to download cytoband file from UCSC ftp. If there is no cytoband file
available for the species, there will be an error.
}
\value{
No value is returned.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
gtrellis_layout(n_track = 2, ncol = 3,
    track_height = unit.c(unit(1, "null"), unit(5, "mm")))
add_ideogram_track(track = 2)

}
