\name{add_ideogram_track}
\alias{add_ideogram_track}
\title{
add ideogram track  


}
\description{
add ideogram track  


}
\usage{
add_ideogram_track(species = NULL, i_track = get_cell_meta_data("i_track") + 1)
}
\arguments{

  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download \code{cytoBand.txt.gz} from UCSC ftp automatically.}
  \item{i_track}{which track the ideogram is added. By default it is the next track in the layout.}

}
\details{
The function tries to download cytoband file from UCSC ftp. If there is no cytoband file for some species, there will be error.  


}
\value{
no value returned  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
initialize_layout(n_track = 2, ncol = 3,
    track_height = unit.c(unit(1, "null"), unit(5, "mm")))
add_ideogram_track(i_track = 2)
}
