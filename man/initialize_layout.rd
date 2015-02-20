\name{initialize_layout}
\alias{initialize_layout}
\title{
Initialize genome-level Trellis layout  


}
\description{
Initialize genome-level Trellis layout  


}
\usage{
initialize_layout(data = NULL, category = NULL,
    species = NULL, nrow = NULL, ncol = NULL,
    n_track = 1, track_height = 1, track_ylim = c(0, 1),
    track_axis = TRUE, track_ylab = "",
    main = NULL, xlab = "Genomic positions", xaxis = TRUE,
    equal_width = FALSE, border = TRUE, asist_ticks = TRUE,
    xpadding = c(0, 0), ypadding = c(0, 0), gap = unit(1, "mm"),
    byrow = TRUE, newpage = TRUE, add_name_track = FALSE,
    name_fontsize = 10, name_track_fill = "#EEEEEE",
    add_ideogram_track = FALSE, ideogram_track_height = unit(2, "mm"),
    axis_label_fontsize = 6, lab_fontsize = 10, main_fontsize = 16)
}
\arguments{

  \item{data}{a data frame with at least three columns. The first three columns are genomic categories (e.g. chromosomes),  start positions and end positions. This data frame is used to extract ranges for each genomic category.}
  \item{category}{subset of categories. It is also used for ordering.}
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download \code{chromInfo.txt.gz} from UCSC ftp automatically.}
  \item{nrow}{Number of rows in the layout}
  \item{ncol}{Number of columns in the layout}
  \item{n_track}{Number of tracks}
  \item{track_height}{height of tracks, it should be numeric which means the value is relative or \code{\link[grid]{unit}} object}
  \item{track_ylim}{track ylim, can be a vector of length two, or a vector of length \code{2*n_track} or a matrix with two columns}
  \item{track_axis}{track axis, whether showing y-axes for tracks}
  \item{track_ylab}{track label, \code{''} means there is no label for the track}
  \item{main}{title of the plot}
  \item{xlab}{labels on x-axis}
  \item{xaxis}{whether showing x-axis}
  \item{equal_width}{whether all columns have the same width}
  \item{border}{whether showing borders}
  \item{asist_ticks}{if axes ticks are added on one side in rows or columns, whether to add ticks on the other side}
  \item{xpadding}{xpadding, numeric value means relative ratio to the cell width. use \code{\link[base]{I}} to set it as absolute value which is measured in the datavp. Currnetly you cannot set it as a \code{\link[grid]{unit}} object.}
  \item{ypadding}{ypadding, only numeric value}
  \item{gap}{0 or a \code{\link[grid]{unit}} object. If it is length 2, the first one corresponds to the gaps between rows and the seond corresponds to the gaps between columns}
  \item{byrow}{arrange categories (e.g. chromosomes) by rows or by columns}
  \item{newpage}{whether to call \code{\link[grid]{grid.newpage}} to create a new page}
  \item{add_name_track}{whether add a pre-defined name track (insert before the first track)}
  \item{name_fontsize}{font size for text in the name track. Note the font size also affects the height of name track}
  \item{name_track_fill}{filled color for name track}
  \item{add_ideogram_track}{whether to add a pre-defined ideogram track (insert after the last track).}
  \item{ideogram_track_height}{height of ideogram track}
  \item{axis_label_fontsize}{font size for axis labels}
  \item{lab_fontsize}{font size for x-labels and y-labels}
  \item{main_fontsize}{font size for title}

}
\details{
Genome level Trellis graph visualizes genomic data conditioned by genomic categories (e.g. chromosomes). For each chromosome, there would be multiple dimensional data which describe certain features in the chromosome from different aspects. The \code{\link{initialize_layout}} function arranges chromosomes on the plot based on certain rules. Then users can apply \code{\link{add_track}} to add self-defined graphics to the plot track by track.  

For more detailed demonstration of the function, please go to the vignette.  


}
\value{
no value returned  


}
\seealso{
\code{\link{add_track}}, \code{\link{add_ideogram_track}}  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
initialize_layout()
initialize_layout(ncol = 5)
initialize_layout(n_track = 3, ncol = 4)

# for more examples, please go to the vignette
}
