\name{gtrellis_layout}
\alias{gtrellis_layout}
\title{
Initialize genome level Trellis layout  


}
\description{
Initialize genome level Trellis layout  


}
\usage{
gtrellis_layout(data = NULL, category = NULL,
    species = NULL, nrow = NULL, ncol = NULL,
    n_track = 1, track_height = 1, track_ylim = c(0, 1),
    track_axis = TRUE, track_ylab = "",
    title = NULL, xlab = "Genomic positions", xaxis = TRUE,
    equal_width = FALSE, border = TRUE, asist_ticks = TRUE,
    xpadding = c(0, 0), ypadding = c(0, 0), gap = unit(1, "mm"),
    byrow = TRUE, newpage = TRUE, add_name_track = FALSE,
    name_fontsize = 10, name_track_fill = "#EEEEEE",
    add_ideogram_track = FALSE, ideogram_track_height = unit(2, "mm"),
    axis_label_fontsize = 6, lab_fontsize = 10, title_fontsize = 16)
}
\arguments{

  \item{data}{a data frame with at least three columns. The first three columns are genomic categories (e.g. chromosomes),  start positions and end positions. This data frame is used to extract ranges for each genomic category.}
  \item{category}{subset of categories. It is also used for ordering.}
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download \code{chromInfo.txt.gz} from UCSC ftp automatically. Short scaffolds will be removed. Pass to \code{\link[circlize]{read.chromInfo}}.}
  \item{nrow}{Number of rows in the layout.}
  \item{ncol}{Number of columns in the layout.}
  \item{n_track}{Number of tracks for each genomic category.}
  \item{track_height}{height of tracks. It should be numeric which means the value is relative or a \code{\link[grid]{unit}} object.}
  \item{track_ylim}{ranges on y axes of tracks. The value can be a vector of length two which means all tracks share same y ranges, or a matrix with two columns or a vector of length \code{2*n_track} which will be transformed into the two-column matrix by rows.}
  \item{track_axis}{whether show y axes for tracks. The value is logical that can be either length one or number of tracks.}
  \item{track_ylab}{labels for tracks on y axes. The value can be either length one or number of tracks.}
  \item{title}{title of the plot.}
  \item{xlab}{labels on x axes.}
  \item{xaxis}{whether show x axes.}
  \item{equal_width}{whether all columns in the layout have the same width. If \code{TRUE}, short categories will be extended according to the longest category.}
  \item{border}{whether show borders.}
  \item{asist_ticks}{if axes ticks are added on one side in rows or columns, whether add ticks on the other side.}
  \item{xpadding}{padding on x axes in each cell. Numeric value means relative ratio corresponding to the cell width.  Use \code{\link[base]{I}} to set it as absolute value which is measured in the data viewport (the coordinate system corresponding to the real data). Currnetly you cannot set it as a \code{\link[grid]{unit}} object.}
  \item{ypadding}{padding on y axes in each cell. Only numeric value is allowed currently.}
  \item{gap}{0 or a \code{\link[grid]{unit}} object. If it is length two, the first element corresponds to the gaps between rows and the seond corresponds to the gaps between columns.}
  \item{byrow}{arrange categories (e.g. chromosomes) by rows or by columns in the layout.}
  \item{newpage}{whether call \code{\link[grid]{grid.newpage}} to create a new page.}
  \item{add_name_track}{whether add a pre-defined name track (insert before the first track). The name track is simply a track which only contains text, implemented by \code{\link{add_track}}.}
  \item{name_fontsize}{font size for text in the name track. Note the font size also affects the height of name track.}
  \item{name_track_fill}{filled color for name track.}
  \item{add_ideogram_track}{whether to add a pre-defined ideogram track (insert after the last track). If the cytoband data for specified species is not available, this argument is ignored. The ideogram track simply contains rectangels with different colors, implemented by \code{\link{add_track}}.}
  \item{ideogram_track_height}{Height of ideogram track. The value should be a \code{\link[grid]{unit}} object.}
  \item{axis_label_fontsize}{font size for axis labels.}
  \item{lab_fontsize}{font size for x-labels and y-labels.}
  \item{title_fontsize}{font size for title.}

}
\details{
Genome level Trellis graph visualizes genomic data conditioned by genomic categories (e.g. chromosomes). For each genomic category, multiple dimensional data which are represented as tracks describe different features from different aspects. The \code{\link{gtrellis_layout}} function arranges genomic categories on the plot in a quite flexible way. Then users apply \code{\link{add_track}} to add self-defined graphics to the plot track by track.  

For more detailed demonstration of the function, please go to the vignette.  


}
\section{Legend}{
Legend is not supported in this package. But it is easy to add legends based on the \code{grid} graphic system. Following example shows adding a simple legend on the right of the Trellis plot.  

First create a \code{\link[grid]{viewport}} or a \code{\link[grid]{grob}} object that contains the legend. The most simple way is to  use \code{\link[grid]{legendGrob}} to construct a simple legend.  

  \preformatted{
    legd = legendGrob("label", pch = 16)
  }

Create a layout which contains two columns and we set the width for the second column to the width  of the legend by \code{\link[grid]{grobWidth}}.  

  \preformatted{
    layout = grid.layout(nrow = 1, ncol = 2, widths = unit.c(unit(1, "null"), grobWidth(legd)))
    grid.newpage()
    pushViewport(viewport(layout = layout))
  }

In the left column, we add Trellis plot. Here you need to specify \code{newpage} to \code{FALSE} so that the plot is added into the current page which not creating a new one.  

  \preformatted{
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
    gtrellis_layout(nrow = 5, byrow = FALSE, track_ylim = range(bed[[4]]), newpage = FALSE)
    add_track(bed, panel.fun = function(bed) \{
        x = (bed[[2]] + bed[[3]]) / 2
        y = bed[[4]]
        grid.points(x, y, pch = 16, size = unit(0.5, "mm"))
    \})
    upViewport()
  }

In the right column, add the legend.  

  \preformatted{
    pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
    grid.draw(legd)
    upViewport()
  }


}
\value{
No value is returned.  


}
\seealso{
\code{\link{add_track}}, \code{\link{add_ideogram_track}}  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
gtrellis_layout()
gtrellis_layout(ncol = 5)
gtrellis_layout(n_track = 3, ncol = 4)

# for more examples, please go to the vignette
}
