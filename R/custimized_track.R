
# == title
# add points to a new or exsited track
#
# == param
# -gr genomic regions, it can be a data frame or a `GenomicRanges::GRanges` object
# -value numeric values associated with ``gr``
# -pch shape of points
# -size size of points, should be a `grid::unit` object
# -gp graphic settings, should be specified by `grid::gpar`.
# -... other arguments passed to `add_track`
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# require(circlize)
# bed = generateRandomBed()
# gtrellis_layout(track_ylim = range(bed[[4]]), nrow = 3, byrow = FALSE)
# add_points_track(bed, bed[[4]], gp = gpar(col = ifelse(bed[[4]] > 0, "red", "green")))
#
add_points_track = function(gr, value, pch = 16, size = unit(1, "mm"), gp = gpar(), ...) {

	gr = normalizeToGRanges(gr)
	n = length(gr)

	col = if(is.null(gp$col)) get.gpar("col")[[1]] else gp$col
	fill = if(is.null(gp$fill)) get.gpar("fill")[[1]] else gp$fill

	gr$..value.. = value
	gr$..pch.. = if(length(pch) == 1) rep(pch, n) else pch
	gr$..size.. = if(length(size) == 1) rep(size, n) else size
	gr$..col.. = if(length(col) == 1) rep(col, n) else col
	gr$..fill.. = if(length(fill) == 1) rep(fill, n) else fill
	add_track(gr, panel_fun = function(gr) {
		x = (start(gr) + end(gr))/2
		grid.points(x, gr$..value.., default.units = "native", pch = gr$..pch.., size = gr$..size.., gp = gpar(col = gr$..col.., fill = gr$..fill..))
	}, ...)
}

# == title
# add segments to a new or exsited track
#
# == param
# -gr genomic regions, it can be a data frame or a `GenomicRanges::GRanges` object
# -value numeric values associated with ``gr``
# -gp graphic settings, should be specified by `grid::gpar`.
# -... other arguments passed to `add_track`
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# require(circlize)
# bed = generateRandomBed(nr = 100)
# gtrellis_layout(track_ylim = range(bed[[4]]), nrow = 3, byrow = FALSE)
# add_segments_track(bed, bed[[4]], gp = gpar(col = ifelse(bed[[4]] > 0, "red", "green"), lwd = 4))
#
add_segments_track = function(gr, value, gp = gpar(), ...) {

	gr = normalizeToGRanges(gr)
	n = length(gr)

	col = if(is.null(gp$col)) get.gpar("col")[[1]] else gp$col
	lwd = if(is.null(gp$lwd)) get.gpar("lwd")[[1]] else gp$lwd
	lty = if(is.null(gp$lty)) get.gpar("lty")[[1]] else gp$lty
	lineend = "butt"
	
	gr$..value.. = value
	gr$..lwd.. = if(length(lwd) == 1) rep(lwd, n) else lwd
	gr$..col.. = if(length(col) == 1) rep(col, n) else col
	add_track(gr, panel_fun = function(gr) {
		grid.segments(start(gr), gr$..value.., end(gr), gr$..value.., default.units = "native", 
			gp = gpar(col = gr$..col.., lwd = gr$..lwd.., lty = gr$..lty.., lineend = lineend))
	}, ...)
}

# == title
# add lines to a new or exsited track
#
# == param
# -gr genomic regions, it can be a data frame or a `GenomicRanges::GRanges` object
# -value numeric values associated with ``gr``
# -area whether draw polygon for the area under the line
# -baseline baseline for drawing polygon
# -gp graphic settings, should be specified by `grid::gpar`.
# -... other arguments passed to `add_track`
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# require(circlize)
# bed = generateRandomBed(200)
# gtrellis_layout(n_track = 2, track_ylim = rep(range(bed[[4]]), 2), nrow = 3, byrow = FALSE)
# add_lines_track(bed, bed[[4]])
# add_lines_track(bed, bed[[4]], area = TRUE, gp = gpar(fill = "grey", col = NA))
#
add_lines_track = function(gr, value, area = FALSE, baseline = "bottom", gp = gpar(), ...) {
	
	gr = normalizeToGRanges(gr)
	n = length(gr)

	gr$..value.. = value
	add_track(gr, panel_fun = function(gr) {
		x = (start(gr) + end(gr))/2
		if(area) {
			if(baseline == "bottom") {
				baseline = get_cell_meta_data("extended_ylim")[1]
			} else if(baseline == "top") {
				baseline = get_cell_meta_data("extended_ylim")[2]
			}
			grid.polygon(c(x[1], x, x[length(x)]), c(baseline, gr$..value.., baseline), default.units = "native", gp = gp)
		} else {
			grid.lines(x, gr$..value.., default.units = "native", gp = gp)
		}
	}, ...)
}

# == title
# add retangles to a new or exsited track
#
# == param
# -gr genomic regions, it can be a data frame or a `GenomicRanges::GRanges` object
# -h1 top/bottom positions for rectangles
# -h2 top/bottom positions for rectangles
# -gp graphic settings, should be specified by `grid::gpar`.
# -... other arguments passed to `add_track`
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == seealso
# `add_heatmap_track`, `add_track`
#
# == example
# require(circlize)
# bed = generateRandomBed(200)
# col_fun = colorRamp2(c(-1, 0, 1), c("green", "black", "red"))
# gtrellis_layout(track_ylim = range(bed[[4]]), nrow = 3, byrow = FALSE)
# add_rect_track(bed, h1 = bed[[4]], h2 = 0, 
#     gp = gpar(col = NA, fill = col_fun(bed[[4]])))
#
add_rect_track = function(gr, h1, h2, gp = gpar(), ...) {

	gr = normalizeToGRanges(gr)
	n = length(gr)

	col = if(is.null(gp$col)) get.gpar("col")[[1]] else gp$col
	fill = if(is.null(gp$fill)) get.gpar("fill")[[1]] else gp$fill
	lwd = if(is.null(gp$lwd)) get.gpar("lwd")[[1]] else gp$lwd
	lty = if(is.null(gp$lty)) get.gpar("lty")[[1]] else gp$lty

	lineend = "butt"
	linejoin = "mitre"

	if(!inherits(h1, "unit")) h1 = unit(h1, "native")
	if(!inherits(h2, "unit")) h2 = unit(h2, "native")
	
	gr$..h1.. = if(length(h1) == 1) rep(h1, n) else h1
	gr$..h2.. = if(length(h2) == 1) rep(h2, n) else h2
	gr$..lwd.. = if(length(lwd) == 1) rep(lwd, n) else lwd
	gr$..lty.. = if(length(lty) == 1) rep(lty, n) else lty
	gr$..col.. = if(length(col) == 1) rep(col, n) else col
	gr$..fill.. = if(length(fill) == 1) rep(fill, n) else fill

	add_track(gr, panel_fun = function(gr) {
		grid.rect((start(gr) + end(gr))*0.5, (gr$..h1.. + gr$..h2..)*0.5,
			width = end(gr) - start(gr), 
			height = gr$..h1.. - gr$..h2..,
			default.units = "native", gp = gpar(col = gr$..col.., fill = gr$..fill.., lwd = gr$..lwd.., lty = gr$..lty..,
				lineend = lineend, linejoin = linejoin))
	}, ...)
}

# == title
# add heatmap to a new track
#
# == param
# -gr genomic regions, it can be a data frame or a `GenomicRanges::GRanges` object
# -mat matrix in which rows correspond to intervals in ``gr``
# -fill a color mapping function which maps values to colors. Users can consider `circlize::colorRamp2` to generate a color mapping function.
# -border border of the grids in heatmap
# -track which track the graphics will be added to. By default it is the next track. The value should only be a scalar.
# -... other arguments passed to `add_track`
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == seealso
# `add_rect_track`, `add_track`
#
# == example
# require(circlize)
# bed = generateRandomBed(200)
# col_fun = colorRamp2(c(-1, 0, 1), c("green", "black", "red"))
# gtrellis_layout(nrow = 3, byrow = FALSE, track_axis = FALSE)
# mat = matrix(rnorm(nrow(bed)*4), ncol = 4)
# add_heatmap_track(bed, mat, fill = col_fun)
#
add_heatmap_track = function(gr, mat, fill, border = NA, track = current_track() + 1, ...) {

	gr = normalizeToGRanges(gr)
	n = length(gr)

	if(inherits(mat, "data.frame")) {
		mat = as.matrix(mat)
	} else if(!inherits(mat, "matrix")) {
		mat = matrix(mat, ncol = 1)
	}

	col_mat = fill(mat)
	nr = ncol(mat)

	for(i in 1:nr) {
		add_rect_track(gr, h1 = unit(i/nr, "npc"), h2 = unit((i-1)/nr, "npc"), gp = gpar(fill = col_mat[, i], col = border), 
			track = track, ...)
	}
}

normalizeToGRanges = function(data) {
	if(inherits(data, "GRanges")) {
		gr = data
	} else if(inherits(data, "data.frame")) {
		gr = GRanges(seqnames = data[, 1], ranges = IRanges(data[, 2], data[, 3]))
	}
	return(gr)
}
