\name{gtrellis_layout}
\alias{gtrellis_layout}
\title{
Initialize genome-level Trellis layout
}
\description{
Initialize genome-level Trellis layout
}
\usage{
gtrellis_layout(data = NULL, category = NULL,
    species = NULL, nrow = NULL, ncol = NULL,
    n_track = 1, track_height = 1, track_ylim = c(0, 1),
    track_axis = TRUE, track_ylab = "", title = NULL,
    xlab = "Genomic positions", xaxis = TRUE, xaxis_bin = NULL,
    equal_width = FALSE, compact = FALSE, border = TRUE, asist_ticks = TRUE,
    xpadding = c(0, 0), ypadding = c(0, 0), gap = unit(1, "mm"),
    byrow = TRUE, newpage = TRUE, add_name_track = FALSE,
    name_fontsize = 10, name_track_fill = "#EEEEEE",
    add_ideogram_track = FALSE, ideogram_track_height = unit(2, "mm"),
    axis_label_fontsize = 6, lab_fontsize = 10, title_fontsize = 16,
    legend = list(), legend_side = c("right", "bottom"),
    padding = unit(c(2, 2, 2, 2), "mm"), remove_chr_prefix = FALSE)
}
\arguments{

  \item{data}{a data frame with at least three columns. The first three columns should be genomic categories (e.g. chromosomes),  start positions and end positions. This data frame is used to extract ranges for each genomic category (minimal and maximal positions are taken as the range in the corresponding category).}
  \item{category}{subset of categories. It is also used for ordering.}
  \item{species}{Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this value is specified, the function will download \code{chromInfo.txt.gz} from UCSC ftp automatically. Short scaffolds will be removed if they have obvious different length as others. The argument is passed to \code{\link[circlize]{read.chromInfo}}.}
  \item{nrow}{Number of rows in the layout.}
  \item{ncol}{Number of columns in the layout.}
  \item{n_track}{Number of tracks in each genomic category.}
  \item{track_height}{height of tracks. It should be numeric which means the value is relative and will be scaled into percent, or a \code{\link[grid]{unit}} object.}
  \item{track_ylim}{ranges on y axes of tracks. The value can be a vector of length two which means all tracks share same y ranges, or a matrix with two columns, or a vector of length \code{2*n_track} which will be coerced into the two-column matrix by rows.}
  \item{track_axis}{whether show y axes for tracks. The value is logical that can be either length one or number of tracks.}
  \item{track_ylab}{labels for tracks on y axes. The value can be either length one or number of tracks.}
  \item{title}{title of the plot.}
  \item{xlab}{labels on x axes.}
  \item{xaxis}{whether show x axes.}
  \item{xaxis_bin}{bin size for x axes.}
  \item{equal_width}{whether all columns in the layout have the same width. If \code{TRUE}, short categories will be extended according to the longest category.}
  \item{compact}{For the catgories which are put in a same row, will they be put compactly without being aligned by columns.}
  \item{border}{whether show borders.}
  \item{asist_ticks}{if axes ticks are added on one side in rows or columns, whether add ticks on the other sides.}
  \item{xpadding}{padding on x axes in each cell. Numeric value means relative ratio corresponding to the cell width.  Use \code{\link[base]{I}} to set it as absolute value which is measured in the data viewport (the coordinate system corresponding to the real data). Currently you cannot set it as a \code{\link[grid]{unit}} object.}
  \item{ypadding}{padding on y axes in each cell. Only numeric value is allowed currently.}
  \item{gap}{0 or a \code{\link[grid]{unit}} object. If it is length two, the first element corresponds to the gaps between rows and the second corresponds to the gaps between columns.}
  \item{byrow}{arrange categories (e.g. chromosomes) by rows or by columns in the layout.}
  \item{newpage}{whether call \code{\link[grid]{grid.newpage}} to create a new page.}
  \item{add_name_track}{whether add a pre-defined name track (insert before the first track). The name track is simply a track which only contains text. The default style of the name track is simple, but users can self define their own by \code{\link{add_track}}.}
  \item{name_fontsize}{font size for text in the name track. Note the font size also affects the height of name track.}
  \item{name_track_fill}{filled color for name track.}
  \item{add_ideogram_track}{whether to add a pre-defined ideogram track (insert after the last track). If the cytoband data for specified species is not available, this argument is ignored. The ideogram track simply contains rectangles with different colors, implemented by \code{\link{add_track}}.}
  \item{ideogram_track_height}{Height of ideogram track. The value should be a \code{\link[grid]{unit}} object.}
  \item{axis_label_fontsize}{font size for axis labels.}
  \item{lab_fontsize}{font size for x-labels and y-labels.}
  \item{title_fontsize}{font size for title.}
  \item{legend}{a \code{\link[grid]{grob}} object, or a list of \code{\link[grid]{grob}} objects.}
  \item{legend_side}{side of the legend}
  \item{padding}{padding of the plot. Elements correspond to bottom, left, top, right paddings.}
  \item{remove_chr_prefix}{if chromosome names start with 'chr', whether to remove it.}

}
\details{
Genome-level Trellis graph visualizes genomic data conditioned by genomic categories (e.g. chromosomes).
For each genomic category, multiple dimensional data which are represented as tracks describe different features from different
aspects. The \code{\link{gtrellis_layout}} function arranges genomic categories on the plot in a quite flexible way. Then users
apply \code{\link{add_track}} to add self-defined graphics to the plot track by track.

For more detailed demonstration of the function, please refer to the vignette.
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
