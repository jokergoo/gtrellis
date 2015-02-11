\name{initialize_layout}
\alias{initialize_layout}
\title{
initialize genomic trellis layout  


}
\description{
initialize genomic trellis layout  


}
\usage{
initialize_layout(data = NULL, chromosome = NULL,
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

  \item{data}{a data frame, same rule as \code{data} argument in \code{\link[circlize]{circos.genomicInitialize}}.}
  \item{chromosome}{chromosome}
  \item{species}{species}
  \item{nrow}{nrow in the layout}
  \item{ncol}{ncol in the layout}
  \item{n_track}{how many tracks}
  \item{track_height}{height of tracks, should be numeric which means the value is relative or \code{\link[grid]{unit}} class}
  \item{track_ylim}{track ylim, can be a vector of length two, or a vector of length 2*n_track or a matrix}
  \item{track_axis}{track axis, whether show y-axis for a track}
  \item{track_ylab}{track label, \code{''} means there is no label for the track}
  \item{main}{title}
  \item{xlab}{xlab}
  \item{xaxis}{whether add x-axis}
  \item{equal_width}{whether all columns hava the same width}
  \item{border}{draw border}
  \item{asist_ticks}{if axes ticks and labels are added on one side in rows or columns, whether to add ticks on the other side}
  \item{xpadding}{xpadding, numeric value means relative ratio to the cell width. use \code{\link[base]{I}} to set it as absolute value which is measured in the datavp.}
  \item{ypadding}{ypadding, only numeric value}
  \item{gap}{0 or a \code{\link[grid]{unit}} class. If it is length 2, the first one corresponds to the gaps between rows and the seond corresponds to the gaps between columns}
  \item{byrow}{arrange categories (e.g. chromosomes) by rows ?}
  \item{newpage}{whether call \code{\link[grid]{grid.newpage}} to initialize on a new graphic device}
  \item{add_name_track}{whether add a pre-defined name track (insert before the first track)}
  \item{name_fontsize}{font size for cell names, the font size also controls the height of name track}
  \item{name_track_fill}{filled color for name track}
  \item{add_ideogram_track}{whether add a pre-defined ideogram track (insert after the last track).}
  \item{ideogram_track_height}{height of ideogram track}
  \item{axis_label_fontsize}{font size for axis labels}
  \item{lab_fontsize}{font size for x-labels and y-labels}
  \item{main_fontsize}{font size for title}

}
\details{
please go to the vignette. 


}
