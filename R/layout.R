
.GENOMIC_LAYOUT = new.env()

# == title
# Initialize genome-level Trellis layout
#
# == param
# -data a data frame with at least three columns. The first three columns should be genomic categories (e.g. chromosomes), 
#       start positions and end positions. This data frame is used to extract ranges for each genomic category (minimal
#       and maximal positions are taken as the range in the corresponding category).
# -category subset of categories. It is also used for ordering.
# -species Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download ``chromInfo.txt.gz`` from
#          UCSC ftp automatically. Short scaffolds will be removed if they have obvious different length as others. The argument is passed
#          to `circlize::read.chromInfo`.
# -nrow Number of rows in the layout.
# -ncol Number of columns in the layout.
# -n_track Number of tracks in each genomic category.
# -track_height height of tracks. It should be numeric which means the value is relative and will be scaled into percent, or a `grid::unit` object.
# -track_ylim ranges on y axes of tracks. The value can be a vector of length two which means all tracks share same
#             y ranges, or a matrix with two columns, or a vector of length ``2*n_track`` which will be coerced
#             into the two-column matrix by rows.
# -track_axis whether show y axes for tracks. The value is logical that can be either length one or number of tracks.
# -track_ylab labels for tracks on y axes. The value can be either length one or number of tracks.
# -title title of the plot.
# -xlab labels on x axes.
# -xaxis whether show x axes.
# -equal_width whether all columns in the layout have the same width. If ``TRUE``, short categories will be extended
#              according to the longest category.
# -border whether show borders.
# -asist_ticks if axes ticks are added on one side in rows or columns, whether add ticks on the other sides.
# -xpadding padding on x axes in each cell. Numeric value means relative ratio corresponding to the cell width. 
#           Use `base::I` to set it as absolute value which is measured in the data viewport (the coordinate system corresponding
#           to the real data). Currently you cannot set it as a `grid::unit` object.
# -ypadding padding on y axes in each cell. Only numeric value is allowed currently.
# -gap 0 or a `grid::unit` object. If it is length two, the first element corresponds to the gaps between rows and
#      the second corresponds to the gaps between columns.
# -byrow arrange categories (e.g. chromosomes) by rows or by columns in the layout.
# -newpage whether call `grid::grid.newpage` to create a new page.
# -add_name_track whether add a pre-defined name track (insert before the first track). The name track is simply a track
#                 which only contains text. The default style of the name track is simple, but users can
#                 self define their own by `add_track`.
# -name_fontsize font size for text in the name track. Note the font size also affects the height of name track.
# -name_track_fill filled color for name track.
# -add_ideogram_track whether to add a pre-defined ideogram track (insert after the last track). If the cytoband data for specified
#                     species is not available, this argument is ignored. The ideogram track simply contains rectangles
#                     with different colors, implemented by `add_track`.
# -ideogram_track_height Height of ideogram track. The value should be a `grid::unit` object.
# -axis_label_fontsize font size for axis labels.
# -lab_fontsize font size for x-labels and y-labels.
# -title_fontsize font size for title.
#
# == detail
# Genome-level Trellis graph visualizes genomic data conditioned by genomic categories (e.g. chromosomes).
# For each genomic category, multiple dimensional data which are represented as tracks describe different features from different
# aspects. The `gtrellis_layout` function arranges genomic categories on the plot in a quite flexible way. Then users
# apply `add_track` to add self-defined graphics to the plot track by track.
#
# For more detailed demonstration of the function, please go to the vignette.
#
# == legend
# Legend is not supported in this package. But it is easy to add legends based on the ``grid`` graphic system.
# Following example shows adding a simple legend on the right of the Trellis plot.
#
# First create a `grid::grob` object that contains the legend. The most simple way is to 
# use `grid::legendGrob` to construct a simple legend.
#
#     legd = legendGrob("label", pch = 16)
#
# Create a layout which contains two columns and we set the width for the second column to the width 
# of the legend by `grid::grobWidth`.
#
#     layout = grid.layout(nrow = 1, ncol = 2, widths = unit.c(unit(1, "null"), grobWidth(legd)))
#     grid.newpage()
#     pushViewport(viewport(layout = layout))
#
# In the left column, we add Trellis plot. Here you need to specify ``newpage`` to ``FALSE`` so that
# the plot is added into the current page which not creating a new one.
#
#     pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
#     gtrellis_layout(nrow = 5, byrow = FALSE, track_ylim = range(bed[[4]]), newpage = FALSE)
#     add_track(bed, panel.fun = function(bed) {
#         x = (bed[[2]] + bed[[3]]) / 2
#         y = bed[[4]]
#         grid.points(x, y, pch = 16, size = unit(0.5, "mm"))
#     })
#     upViewport()
#
# In the right column, add the legend.
#
#     pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
#     grid.draw(legd)
#     upViewport()
#
# == value
# No value is returned.
#
# == seealso
# `add_track`, `add_ideogram_track`
#
# == authors
# Zuguang Gu <z.gu@dkfz.de>
#
gtrellis_layout = function(data = NULL, category = NULL, 
    species = NULL, nrow = NULL, ncol = NULL,
    n_track = 1, track_height = 1, track_ylim = c(0, 1),
    track_axis = TRUE, track_ylab = "", 
    title = NULL, xlab = "Genomic positions", xaxis = TRUE,
    equal_width = FALSE, border = TRUE, asist_ticks = TRUE,
    xpadding = c(0, 0), ypadding = c(0, 0), gap = unit(1, "mm"),
    byrow = TRUE, newpage = TRUE, add_name_track = FALSE, 
    name_fontsize = 10, name_track_fill = "#EEEEEE",
    add_ideogram_track = FALSE, ideogram_track_height = unit(2, "mm"), 
    axis_label_fontsize = 6, lab_fontsize = 10, title_fontsize = 16) {

    increase_plot_index()
    i_plot = get_plot_index()

    op = qq.options(READ.ONLY = FALSE)
    on.exit(qq.options(op))
    qq.options(code.pattern = "@\\{CODE\\}")

    if(length(track_height) == 1) {
        if(is.unit(track_height)) {
            track_height = do.call("unit.c", lapply(seq_len(n_track), function(i) track_height))
        } else {
            track_height = rep(track_height, n_track)
        }
    }

    if(length(track_ylim) == 2) {
        track_ylim = do.call("rbind", rep(list(track_ylim), n_track))
    }

    if(length(track_axis) == 1) {
        track_axis = rep(track_axis, n_track)
    }

    ## start from xlim
    if(is.null(data)) {
        if(is.null(category)) {
            chromInfo = read.chromInfo(species = species)
            chr_len = sort(chromInfo$chr.len, decreasing = TRUE)

            # sometimes there are small scaffold
            i = which(chr_len[seq_len(length(chr_len)-1)] / chr_len[seq_len(length(chr_len)-1)+1] > 5)[1]
            if(length(i)) {
                chromosome = chromInfo$chromosome[chromInfo$chromosome %in% names(chr_len[chr_len >= chr_len[i]])]
            } else {
                chromosome = chromInfo$chromosome
            }

            category = chromosome
        }
        chromInfo = read.chromInfo(species = species, chromosome.index = category)
        df = chromInfo$df
        category = chromInfo$chromosome
  
        xlim = df[2:3]
        fa = category
    } else {
        data = as.data.frame(data)
        if(is.factor(data[[1]])) {
            fa = levels(data[[1]])
        } else {
            fa = unique(data[[1]])
        }
        
        # calculate xlim
        x1 = tapply(data[[2]], data[[1]], min)[fa]
        x2 = tapply(data[[3]], data[[1]], max)[fa]
        xlim = cbind(x1, x2)
    }
    if(is.null(xlab)) xlab = ""
    if(is.na(xlab)) xlab = ""

    if(is.null(title)) title = ""
    if(is.na(title)) title = ""

    axis_tick_height = unit(1, "mm")
    axis_label_gap = unit(1, "mm")

    # check track*
    if(is.unit(track_height)) {
        if(length(track_height) != n_track) {
            stop(qq("Length of `track_height` should be @{n_track} if it is a `unit`\n"))
        }
    } else {
        if(length(track_height) == 1) {
            track_height = rep(1, n_track)
        } else if(length(track_height) != n_track) {
            stop(qq("Length of `track_height` should be either 1 or @{n_track}\n"))
        }
        if(add_name_track || add_ideogram_track) {
            track_height = unit(track_height/sum(track_height), "null")
        } else {
            track_height = unit(track_height/sum(track_height), "npc")
        }
    }
    if(any(inherits(track_ylim, c("matrix", "data.frame")))) {
        if(nrow(track_ylim) == 1) {
            track_ylim = do.call("rbind", rep(list(c(track_ylim[1, 1], track_ylim[1, 2])), n_track))

        } else if(nrow(track_ylim) != n_track) {
            stop(qq("nrow of `track_ylim` should be either 1 or @{n_track}\n"))
        }
    } else {
        if(length(track_ylim) == 2) {
            track_ylim = do.call("rbind", rep(list(track_ylim[1:2]), n_track))
        } else if(length(track_ylim) == 2*n_track) {
            track_ylim = matrix(track_ylim, ncol = 2, byrow = TRUE)
        } else {
            stop(qq("If `track_ylim` is atomic, the length should be either 2 or @{2*n_track}\n"))
        }
    }
    track_axis = as.logical(track_axis)
    if(length(track_axis) == 1) {
        track_axis = rep(track_axis, n_track)
    } else if(length(track_axis) != n_track) {
        stop(qq("Length of `track_axis` should be either 1 or @{n_track}\n"))
    }
    if(is.null(track_ylab)) track_ylab = ""
    if(length(track_ylab) == 1) {
        track_ylab = rep(track_ylab, n_track)
    } else if(length(track_ylab) != n_track) {
        stop(qq("Length of `track_ylab` should be either 1 or @{n_track}\n"))
    }
    track_ylab[is.na(track_ylab)] = ""

    if(add_name_track) {
        track_height = unit.c(2*max(grobHeight(textGrob(fa, gp = gpar(fontsize = name_fontsize)))), track_height)
        n_track = n_track + 1
        track_ylim = rbind(c(0, 1), track_ylim)
        track_axis = c(FALSE, track_axis)
        track_ylab = c("", track_ylab)
    }
    if(add_ideogram_track) {
        track_height = unit.c(track_height, ideogram_track_height)
        n_track = n_track + 1
        track_ylim = rbind(track_ylim, c(0, 1))
        track_axis = c(track_axis, FALSE)
        track_ylab = c(track_ylab, "")
    }


    n = nrow(xlim)
    if(is.null(nrow) && is.null(ncol)) nrow = 1
    if(!is.null(nrow) && is.null(ncol)) {
        ncol = ceiling(n/nrow)
    } else if(is.null(nrow) && !is.null(ncol)) {
        nrow = ceiling(n/ncol)
    }
    
    if(nrow*ncol > n) {
        fa = c(fa, paste0(".invisible_", seq_len(nrow*ncol-n)))
        for(i in seq_len(nrow*ncol-n)) {
            xlim = rbind(xlim, c(Inf, -Inf))
        }
        n = nrow*ncol
    }
    rownames(xlim) = fa
    fa = as.vector(t(matrix(fa, nrow, ncol, byrow = byrow)))
    xlim = xlim[fa, , drop = FALSE]

    if(!check_xlim(xlim, nrow, ncol)) {
        stop("start base in the same column should be the same.")
    }
    
    .GENOMIC_LAYOUT$fa = fa
    .GENOMIC_LAYOUT$nrow = nrow
    .GENOMIC_LAYOUT$ncol = ncol
    .GENOMIC_LAYOUT$n_track = n_track
    .GENOMIC_LAYOUT$track_axis_show = track_axis
    .GENOMIC_LAYOUT$current_fa = NULL
    .GENOMIC_LAYOUT$current_track = 0
    
    xlim2 = re_calculate_xlim(xlim, nrow, ncol, equal_width)
    
    .GENOMIC_LAYOUT$original_xlim = xlim
    .GENOMIC_LAYOUT$original_ylim = track_ylim
    .GENOMIC_LAYOUT$xlim = xlim2
    .GENOMIC_LAYOUT$ylim = track_ylim

    # if xpadding = I(c(100000, 100000)), it means pad with absolute value
    extended_xlim = xlim2
    extended_track_ylim = track_ylim
    if(class(xpadding) == "AsIs") {
        extended_xlim[, 1] = xlim2[, 1] - xpadding[1]
        extended_xlim[, 2] = xlim2[, 2] + xpadding[2]
    } else {
        extended_xlim[, 1] = xlim2[, 1] - (xlim2[, 2] - xlim2[, 1])*xpadding[1]
        extended_xlim[, 2] = xlim2[, 2] + (xlim2[, 2] - xlim2[, 1])*xpadding[2]
    }

    extended_track_ylim[, 1] = track_ylim[, 1] - (track_ylim[, 2] - track_ylim[, 1])*ypadding[1]
    extended_track_ylim[, 2] = track_ylim[, 2] + (track_ylim[, 2] - track_ylim[, 1])*ypadding[2]

    .GENOMIC_LAYOUT$extended_xlim = extended_xlim
    .GENOMIC_LAYOUT$extended_ylim = extended_track_ylim
    
    
    if(any(sapply(1:ncol, function(x) is_on_top(1, 1, x, nrow, ncol, n_track)))) {
        xaxis_top_height = grobHeight(textGrob("B", gp = gpar(axis_label_fontsize))) + axis_tick_height + axis_label_gap
    } else {
        xaxis_top_height = unit(0, "null")
    }
    if(any(sapply(1:ncol, function(x) is_on_bottom(n_track, nrow, x, nrow, ncol, n_track)))) {
        xaxis_bottom_height = grobHeight(textGrob("B", gp = gpar(axis_label_fontsize))) + axis_tick_height + axis_label_gap
    } else {
        xaxis_bottom_height = unit(0, "null")
    }
    if(xlab == "") {
        xlabel_height = unit(2, "mm")
    } else {
        xlabel_height = grobHeight(textGrob(xlab, gp = gpar(lab_fontsize)))*2
    }

    # which tracks will have anno on left
    lstr = ""
    lstr_ylab = ""
    lstr_width = unit(0, "mm")
    lstr_ylab_height = unit(0, "mm")
    for(i in seq_len(nrow)) {
        for(k in seq_len(n_track)) {
            if(is_on_left(k, i, 1, nrow, ncol, n_track, track_axis | track_ylab != "")) {
                if(track_axis[k]) {
                    range = track_ylim[k, ]
                    axis_label= as.character(grid.pretty(range))
                    for(ax in axis_label) {
                        lstr_width = max(unit.c(lstr_width, grobWidth(textGrob(ax, gp = gpar(fontsize = axis_label_fontsize)))))
                    }
                    if(any(axis_label != "")) lstr = "a"
                }
                lstr_ylab_height = max(unit.c(lstr_ylab_height, grobHeight(textGrob(track_ylab[k], gp = gpar(fontsize = lab_fontsize)))))
                if(track_ylab[k] != "") lstr_ylab = "a"
            }
        }
    }
    if(lstr == "") {
        yaxis_left_width = unit(0, "null")
    } else {
        yaxis_left_width = lstr_width + axis_tick_height + axis_label_gap
    }
    if(lstr_ylab == "") {
        ylabel_left_width = unit(2, "mm")
    } else {
        ylabel_left_width = lstr_ylab_height*2
    }

    lstr = ""
    lstr_ylab = ""
    lstr_width = unit(0, "mm")
    lstr_ylab_height = unit(0, "mm")
    for(i in seq_len(nrow)) {
        for(k in seq_len(n_track)) {
            if(is_on_right(k, i, ncol, nrow, ncol, n_track, track_axis | track_ylab != "")) {
                if(track_axis[k]) {
                    range = track_ylim[k, ]
                    axis_label= as.character(grid.pretty(range))
                    for(ax in axis_label) {
                        lstr_width = max(unit.c(lstr_width, grobWidth(textGrob(ax, gp = gpar(fontsize = axis_label_fontsize)))))
                    }
                    if(any(axis_label != "")) lstr = "a"
                }
                lstr_ylab_height = max(unit.c(lstr_ylab_height, grobHeight(textGrob(track_ylab[k], gp = gpar(fontsize = lab_fontsize)))))
                if(track_ylab[k] != "") lstr_ylab = "a"
            }
        }
    }
    if(lstr == "") {
        yaxis_right_width = unit(0, "null")
    } else {
        yaxis_right_width = lstr_width + axis_tick_height + axis_label_gap
    }
    if(lstr_ylab == "") {
        ylabel_right_width = unit(2, "mm")
    } else {
        ylabel_right_width = lstr_ylab_height*2
    }

    if(title == "") {
        title_height = unit(2, "mm")
    } else {
        title_height = grobHeight(textGrob(title, gp = gpar(fontface = "bold", fontsize = title_fontsize)))*1.5
    }

    if(newpage) grid.newpage(recording = FALSE)
    layout = grid.layout(nrow = 5, ncol = 5, widths = unit.c(ylabel_left_width, yaxis_left_width, unit(1, "null"), yaxis_right_width, ylabel_right_width),
                                             heights = unit.c(title_height, xaxis_top_height, unit(1, "null"), xaxis_bottom_height, xlabel_height))
    pushViewport(viewport(layout = layout, name = qq("global_layout_@{i_plot}")))
    
    if(!is.null(title)) {
        pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 3))
        grid.text(title, gp = gpar(fontface = "bold", fontsize = title_fontsize))
        upViewport()
    }
    
    if(!is.null(xlab)) {
        pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 3))
        grid.text(xlab, gp = gpar(fontsize = lab_fontsize))
        upViewport()
    }
    
    if(length(gap) == 1) {
        gap = rep(gap, 2)
    }
    if(length(gap) == 2) {
        if(!is.unit(gap)) {
            if(all(gap %in% 0)) {
                gap = unit(c(0, 0), "null")
            } else {
                stop("`gap` can only be 0 or unit")
            }
        }
    } else {
        stop("`gap` can only be length of 1 or 2")
    }

    xgap = gap[2]
    ygap = gap[1]

    # initialize each fa
    pushViewport(viewport(layout.pos.col = 3, layout.pos.row = 3, name = qq("title_container_@{i_plot}")))
    
    if(equal_width) {
        chr_width = unit(1, "npc")*(1/ncol) - (ncol - 1)*xgap*(1/ncol)
        chr_x = (1:ncol - 1)*xgap + (1:ncol - 0.5) * chr_width
        chr_width = rep(chr_width, ncol)
    } else {
        ratio = (extended_xlim[, 2] - extended_xlim[, 1])/sum(extended_xlim[, 2] - extended_xlim[, 1])
        chr_width = unit(1, "npc")*ratio - (ncol - 1)*xgap*ratio
        chr_x = NULL
        for(i in seq_along(chr_width)) {
            if(i == 1) {
                chr_x[[i]] = chr_width[i]*0.5
            } else {
                chr_x[[i]] = (i - 1)*xgap + sum(chr_width[seq_len(i-1)]) + chr_width[i]*0.5
            }
        }
        chr_x = do.call("unit.c", chr_x)
    }
    chr_height = unit(1, "npc")*(1/nrow) - (nrow - 1)*ygap*(1/nrow)
    chr_y = (nrow:1 - 1)*ygap + (nrow:1 - 0.5) * chr_height

    # arrange fas on the plot
    for(i in seq_len(nrow)) {
        for(j in seq_len(ncol)) {
            pushViewport(viewport(x = chr_x[j], y = chr_y[i], width = chr_width[j], height = chr_height, name = qq("@{fa[j + (i-1)*ncol]}_container_@{i_plot}")))
            if(is_visible(i, j) && border) grid.rect()
            upViewport()
        }
    }
    upViewport()
    
    # add tracks
    for(i in seq_len(nrow)) {
        for(j in seq_len(ncol)) {
            seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_container_@{i_plot}"))
            layout = grid.layout(nrow = n_track, ncol = 1, heights = track_height)
            pushViewport(viewport(layout = layout, name = qq("@{fa[j + (i-1)*ncol]}_layout_@{i_plot}")))
            for(k in seq_len(n_track)) {
                
                pushViewport(viewport(layout.pos.col = 1, layout.pos.row = k, name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_@{i_plot}")))
                pushViewport(plotViewport(margins = c(0, 0, 0, 0), name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_plotvp_@{i_plot}")))
                pushViewport(dataViewport(xscale = extended_xlim[j, ], yscale = extended_track_ylim[k, ], extension = 0, name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_datavp_@{i_plot}")))
                pushViewport(dataViewport(xscale = extended_xlim[j, ], yscale = extended_track_ylim[k, ], extension = 0, clip = TRUE, name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_datavp_clip_@{i_plot}")))
                if(is_visible(i, j) && border && k > 1) grid.lines(c(0, 1), c(1, 1))
                upViewport(4)
            }
            upViewport()
        }
    }
    seekViewport(name = qq("global_layout_@{i_plot}"))

    # y-labels on the left
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
    for(i in seq_len(nrow)) {
        pushViewport(viewport(y = chr_y[i], height = chr_height, name = qq("ylab_row_@{i}_left_@{i_plot}")))
        
        layout = grid.layout(nrow = n_track, ncol = 1, heights = track_height)
        pushViewport(viewport(layout = layout))
        for(k in seq_len(n_track)) {
                
            pushViewport(viewport(layout.pos.col = 1, layout.pos.row = k, name = qq("ylab_row_@{i}_left_track_@{k}_@{i_plot}")))
            if(is_on_left(k, i, 1, nrow, ncol, n_track, track_axis | track_ylab != "") && is_visible(i, 1)) {
                if(track_ylab[k] != "") {
                    grid.text(track_ylab[k], rot = 90, gp = gpar(fontsize = lab_fontsize))
                }
            }
            upViewport()
        }
        upViewport()

        upViewport()
    }
    upViewport()

    # y-labels on the right
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 5))
    for(i in seq_len(nrow)) {
        pushViewport(viewport(y = chr_y[i], height = chr_height, name = qq("ylab_row_@{i}_right_@{i_plot}")))
        
        layout = grid.layout(nrow = n_track, ncol = 1, heights = track_height)
        pushViewport(viewport(layout = layout))
        for(k in seq_len(n_track)) {
                
            pushViewport(viewport(layout.pos.col = 1, layout.pos.row = k, name = qq("ylab_row_@{i}_right_track_@{k}_@{i_plot}")))
            if(is_on_right(k, i, ncol, nrow, ncol, n_track, track_axis | track_ylab != "") && is_visible(i, ncol)) {
                if(track_ylab[k] != "") {
                    grid.text(track_ylab[k], rot = 90, gp = gpar(fontsize = lab_fontsize))
                }
            }
            upViewport()
        }
        upViewport()

        upViewport()
    }
    upViewport()
    
    basepair_unit = function(x) {
        if(max(x) > 1e6) {
            paste0(x/1e6, "MB")
        } else if(max(x) > 1e3) {
            paste0(x/1e3, "KB")
        } else {
            paste0(x, "bp")
        }
    }

    # x-axis on top
    if(xaxis) {
        il = which.max(xlim2[, 2] - xlim2[, 1])[1]
        breaks = grid.pretty(c(xlim2[il, 1], xlim2[il, 2]))
        i = 1
        for(j in seq_len(ncol)) {
            if(is_visible(i, j)) {
                seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_track_1_datavp_@{i_plot}"))
                xbreaks = seq(grid.pretty(xlim2[j, ])[1], xlim2[j, 2], by = breaks[2]-breaks[1])
                if(length(xbreaks) == 0) xbreaks = grid.pretty(xlim2[j, 1])[1]
                #xbreaks = grid.pretty(xlim2[j, ])
                    
                if(is_on_top(1, i, j, nrow, ncol, n_track)) {
                    label = basepair_unit(xbreaks)

                    pre_end_pos = -Inf
                    for(k in seq_along(label)) {
                        cur_start_pos = unit(xbreaks[k], "native") - 
                                        convertUnit(grobWidth(textGrob(label[k], gp = gpar(fontsize = axis_label_fontsize)))*0.5, "native")
                        cur_start_pos = as.numeric(convertUnit(cur_start_pos, "cm"))
                        if(k == 1 || cur_start_pos > pre_end_pos) {
                            grid.text(label[k], xbreaks[k], unit(1, "npc") + axis_tick_height + axis_label_gap, 
                                default.units = "native", just = "bottom", gp = gpar(fontsize = axis_label_fontsize))
                            pre_end_pos = convertUnit(unit(xbreaks[k], "native"), "npc") + 
                                        convertUnit(grobWidth(textGrob(label[k], gp = gpar(fontsize = axis_label_fontsize)))*0.5, "npc")
                            pre_end_pos = as.numeric(convertUnit(pre_end_pos, "cm"))
                        }
                    }
                    grid.segments(xbreaks, unit(1, "npc") + axis_tick_height,
                              xbreaks, unit(1, "npc"), default.units = "native")
                } else if(asist_ticks) {
                    grid.segments(xbreaks, unit(1, "npc") + axis_tick_height,
                              xbreaks, unit(1, "npc"), default.units = "native")
                }
            }
        }
        
        # x-axis on bottom
        i = nrow
        for(j in seq_len(ncol)) {
            if(is_visible(i, j)) {
                seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_track_@{n_track}_datavp_@{i_plot}"))
                xbreaks = seq(grid.pretty(xlim2[j, ])[1], xlim2[j, 2], by = breaks[2]-breaks[1])
                #xbreaks = grid.pretty(xlim2[j, ])
                
                if(is_on_bottom(n_track, i, j, nrow, ncol, n_track)) {
                    label = basepair_unit(xbreaks)

                    pre_end_pos = -Inf
                    for(k in seq_along(label)) {
                        cur_start_pos = unit(xbreaks[k], "native") - 
                                        convertUnit(grobWidth(textGrob(label[k], gp = gpar(fontsize = axis_label_fontsize)))*0.5, "native")
                        cur_start_pos = as.numeric(convertUnit(cur_start_pos, "cm"))
                        if(k == 1 || cur_start_pos > pre_end_pos) {
                            grid.text(label[k], xbreaks[k], unit(0, "npc") - axis_tick_height - axis_label_gap, 
                                default.units = "native", just = "top", gp = gpar(fontsize = axis_label_fontsize))
                            pre_end_pos = convertUnit(unit(xbreaks[k], "native"), "npc") + 
                                        convertUnit(grobWidth(textGrob(label[k], gp = gpar(fontsize = axis_label_fontsize)))*0.5, "npc")
                            pre_end_pos = as.numeric(convertUnit(pre_end_pos, "cm"))
                        }
                    }
                    grid.segments(xbreaks, unit(0, "npc") - axis_tick_height,
                              xbreaks, unit(0, "npc"), default.units = "native")
                } else if(asist_ticks) {
                    grid.segments(xbreaks, unit(0, "npc") - axis_tick_height,
                              xbreaks, unit(0, "npc"), default.units = "native")
                }
            } else if(asist_ticks) {
                i2 = i
                while(1) {
                    i2 = i2 - 1
                    if(is_visible(i2, j) || i2 == 1) {
                        break
                    }
                }
                seekViewport(name = qq("@{fa[j + (i2-1)*ncol]}_track_@{n_track}_datavp_@{i_plot}"))
                xbreaks = seq(0, xlim2[j, 2], by = 50000000)
                xbreaks = xbreaks[xbreaks >= xlim2[j, 1] & xbreaks <= xlim2[j, 2]]
                grid.segments(xbreaks, unit(0, "npc") - axis_tick_height,
                              xbreaks, unit(0, "npc"), default.units = "native")

            }
        }
    }

    # y-axis on left
    j = 1
    for(i in seq_len(nrow)) {
        for(k in seq_len(n_track)) {
            if(is_visible(i, j)) {
                seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_datavp_@{i_plot}"))
                    ybreaks = grid.pretty(.GENOMIC_LAYOUT$ylim[k, ])
                

                if(is_on_left(k, i, j, nrow, ncol, n_track, track_axis | track_ylab != "")) {
                    if(track_axis[k]) {
                        label = as.character(ybreaks)
                        pre_end_pos = -Inf
                        for(b in seq_along(label)) {
                            cur_start_pos = unit(ybreaks[b], "native") - grobHeight(textGrob(label[b], gp = gpar(fontsize = axis_label_fontsize)))*0.5
                            cur_start_pos = as.numeric(convertUnit(cur_start_pos, "cm", axisFrom = "y"))
                            if(b == 1 || cur_start_pos > pre_end_pos) {
                                grid.text(label[b], unit(0, "npc") - axis_tick_height - axis_label_gap, ybreaks[b],
                                    default.units = "native", just = "right", gp = gpar(fontsize = axis_label_fontsize))
                                pre_end_pos = unit(ybreaks[b], "native") + grobHeight(textGrob(label[b], gp = gpar(fontsize = axis_label_fontsize)))*0.5
                                pre_end_pos = as.numeric(convertUnit(pre_end_pos, "cm", axisFrom = "y"))
                            }
                        }
                    }
                    if(track_axis[k]) {
                        grid.segments(unit(0, "npc") - axis_tick_height, ybreaks,
                                      unit(0, "npc"), ybreaks, default.units = "native")
                    }
                } else if(asist_ticks) {
                    if(track_axis[k]) {
                        grid.segments(unit(0, "npc") - axis_tick_height, ybreaks,
                                      unit(0, "npc"), ybreaks, default.units = "native")
                    }
                }
            }
        }
    }
    
    # y-axis on right
    j = ncol
    for(i in seq_len(nrow)) {
        for(k in seq_len(n_track)) {
            if(is_visible(i, j)) {
                seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_datavp_@{i_plot}"))
                ybreaks = grid.pretty(.GENOMIC_LAYOUT$ylim[k, ])
                

                if(is_on_right(k, i, j, nrow, ncol, n_track, track_axis | track_ylab != "")) {
                    if(track_axis[k]) {
                        label = as.character(ybreaks)
                        pre_end_pos = -Inf
                        for(b in seq_along(label)) {
                            cur_start_pos = unit(ybreaks[b], "native") - grobHeight(textGrob(label[b], gp = gpar(fontsize = axis_label_fontsize)))*0.5
                            cur_start_pos = as.numeric(convertUnit(cur_start_pos, "cm", axisFrom = "y"))
                            if(b == 1 || cur_start_pos > pre_end_pos) {
                                grid.text(label[b], unit(1, "npc") + axis_tick_height + axis_label_gap, ybreaks[b],
                                    default.units = "native", just = "left", gp = gpar(fontsize = axis_label_fontsize))
                                pre_end_pos = unit(ybreaks[b], "native") + grobHeight(textGrob(label[b], gp = gpar(fontsize = axis_label_fontsize)))*0.5
                                pre_end_pos = as.numeric(convertUnit(pre_end_pos, "cm", axisFrom = "y"))
                            }
                        }
                    }
                    if(track_axis[k]) {
                        grid.segments(unit(1, "npc") + axis_tick_height, ybreaks,
                                      unit(1, "npc"), ybreaks, default.units = "native")
                    }
                } else if(asist_ticks) {
                    if(track_axis[k]) {
                        grid.segments(unit(1, "npc") + axis_tick_height, ybreaks,
                                      unit(1, "npc"), ybreaks, default.units = "native")
                    }
                }
            } else if(asist_ticks) {
                j2 = j
                while(1) {
                    j2 = j2 - 1
                    if(is_visible(i, j2) || j2 == 1) {
                        break
                    }
                }
                seekViewport(name = qq("@{fa[j2 + (i-1)*ncol]}_track_@{k}_datavp_@{i_plot}"))
                ybreaks = grid.pretty(.GENOMIC_LAYOUT$ylim[k, ])
                if(track_axis[k]) {
                    grid.segments(unit(1, "npc") + axis_tick_height, ybreaks,
                                  unit(1, "npc"), ybreaks, default.units = "native")
                }
            }
        }
    }
    
    seekViewport(name = qq("global_layout_@{i_plot}"))
    upViewport()

    .GENOMIC_LAYOUT$current_fa = fa[1]
    .GENOMIC_LAYOUT$current_track = 0

    if(add_name_track) {
        add_track(panel.fun = function(gr){
            nm = get_cell_meta_data("name")
            grid.rect(gp = gpar(fill = name_track_fill, col = "#000000"))
            grid.text(nm, gp = gpar(fontsize = name_fontsize))
        })
        .GENOMIC_LAYOUT$current_track = 1
    }

    if(add_ideogram_track) {
        current_track = .GENOMIC_LAYOUT$current_track
        add_ideogram_track(species = species, track = n_track)
        .GENOMIC_LAYOUT$current_track = current_track
    }
    
}

# == title
# Add ideogram track
#
# == param
# -cytoband Path of the cytoband file or a data frame that already contains cytoband data. Pass to `circlize::read.cytoband`.
# -species Abbreviations of species. e.g. hg19 for human, mm10 for mouse. If this
#          value is specified, the function will download ``cytoBand.txt.gz`` from
#          UCSC ftp automatically. Pass to `circlize::read.cytoband`.
# -track which track the ideogram is added in. By default it is the next track in the layout.
#
# == detail
# A track which contains ideograms will be added to the plot. 
#
# The function tries to download cytoband file from UCSC ftp. If there is no cytoband file
# available for the species, there will be an error.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
add_ideogram_track = function(cytoband = paste0(system.file(package = "circlize"),
    "/extdata/cytoBand.txt"), species = NULL, track = get_cell_meta_data("track") + 1) {

	cytoband = read.cytoband(species = species)
    cytoband_df = cytoband$df
    add_track(cytoband_df, track = track, clip = TRUE, panel.fun = function(gr) {
        cytoband_chr = gr
        grid.rect( cytoband_chr[[2]], unit(0, "npc"),
                   width = cytoband_chr[[3]] - cytoband_chr[[2]], height = unit(1, "npc"),
                   default.units = "native", hjust = 0, vjust = 0,
                   gp = gpar(fill = cytoband.col(cytoband_chr[[5]])) )
        grid.rect(min(cytoband_chr[[2]]), unit(0, "npc"),
                  width = max(cytoband_chr[[3]]) - min(cytoband_chr[[2]]), height = unit(1, "npc"),
                  default.units = "native", hjust = 0, vjust = 0,
                  gp = gpar(fill = "transparent"))
    })
}

# == title
# Add self-defined graphics track by track
#
# == param
# -gr genomic regions. It should be a data frame in BED format or a ``GRanges`` object.
# -category subset of categories (e.g. chromosomes) that users want to add graphics. 
#           The value can be a vector which contains more than one category. By default it
#           is all available categories.
# -track which track the graphics will be added to. By default it is the next track. The value should only be a scalar.
# -clip whether graphics are restricted inside the cell.
# -panel.fun self-defined panel function to add graphics in each 'cell'. THe argument ``gr`` in ``panel.fun`` 
#            only contains data for the current category which is a subset of the main ``gr``.
#
# == detail
# Initialization of the Trellis layout and adding graphics are two independent steps.
# Once the layout initialization finished, each cell will be an independent plotting region.
# As same as ``panel.fun`` in `circlize::circlize-package`, the self-defined function ``panel.fun``
# will be applied on every cell in the specified track (by default it is the 'current' track). 
#
# When adding graphics in each cell, `get_cell_meta_data` can return several meta data for the current cell.
#
# Since this package is implemented by the ``grid`` graphic system, ``grid``-family functions
# (such as `grid::grid.points`, `grid::grid.rect`, ...) should be used to add graphics. The usage
# of ``grid`` functions is quite similar as the traditional graphic functions. 
# Followings are several examples:
#
#     grid.points(x, y)
#     grid.lines(x, y)
#     grid.rect(x, y, width, height)
#
# Graphical parameters are usually passed by `grid::gpar`:
#
#     grid.points(x, y, gp = gpar(col = "red")
#     grid.rect(x, y, width, height, gp = gpar(fill = "black", col = "red"))
#
# ``grid`` system also support a large number of coordinate measurement systems by defining proper `grid::unit` object 
# which provides high flexibility to place graphics on the plotting regions.
#
#     grid.points(x, y, default.units = "npc")
#     grid.rect(x, y, width = unit(1, "cm"))
#
# You can refer to the documentations and vignettes of `grid::grid-package` to get a overview.
#
# == value
# No value is returned.
#
# == seealso
# `get_cell_meta_data`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
add_track = function(gr = NULL, category = NULL, track = get_cell_meta_data("track") + 1, 
    clip = TRUE, panel.fun = function(gr) NULL) {

    i_plot = get_plot_index()
    
    op = qq.options(READ.ONLY = FALSE)
    on.exit(qq.options(op))
    qq.options(code.pattern = "@\\{CODE\\}")

    if(length(track) != 1) {
        stop("`track` can only be length 1.\n")
    }
    if(track > .GENOMIC_LAYOUT$n_track || track < 1) {
        stop(qq("`track` should be between [1, @{.GENOMIC_LAYOUT$n_track}]\n"))
    }

    all_fa = .GENOMIC_LAYOUT$fa
    fa = all_fa[ !grepl("^\\.invisible_", all_fa) ]

    if(is.null(category)) {
        if(is.null(gr)) {
            fa = fa
        } else {
            if(inherits(gr, "GenomicRanges")) {
            	if(requireNamespace("GenomicRanges")) {
                	fa = unique(GenomicRanges::seqnames(gr))
                } else {
                	stop("Cannot load `GenomicRanges` package.")
                }
            } else {
                fa = unique(as.character(gr[[1]]))
            }
            if(sum(fa %in% all_fa) == 0) {
                if(sum(grepl("^(\\d+|[xXyY])$", fa)) > 5) {
                    cat("Guess your category are chromosomes and chromosome names should start with 'chr'.\n")
                }
            }

            fa = fa[fa %in% all_fa]

        }
    } else {
        fa = category[category %in% all_fa]
    }
    
    for(chr in fa) {
        .GENOMIC_LAYOUT$current_fa = chr
        .GENOMIC_LAYOUT$current_track = track
        
        if(clip) {
        	vp = qq("@{chr}_track_@{track}_datavp_clip_@{i_plot}")
        } else {
        	vp = qq("@{chr}_track_@{track}_datavp_@{i_plot}")
        }
        if(is.null(gr)) {
            seekViewport(name = vp)
            panel.fun(NULL)
        } else {
            extended_xlim = get_cell_meta_data("extended_xlim")
            if(inherits(gr, "GenomicRanges")) {
            	if(requireNamespace("GenomicRanges")) {
                	sub_gr = GenomicRanges::subset(gr, GenomicRanges::seqnames(gr) == chr)
                } else {
                	stop("Cannot load `GenomicRanges` package.")
                }
                sub_gr = sub_gr[is_intersected(GenomicRanges::start(sub_gr), GenomicRanges::end(sub_gr), extended_xlim[1], extended_xlim[2])]
                if(length(sub_gr)) {
                    seekViewport(name = vp)
                    panel.fun(sub_gr)
                }
            } else {
                sub_gr = gr[gr[[1]] == chr, , drop = FALSE]
                sub_gr = sub_gr[is_intersected(sub_gr[[2]], sub_gr[[3]], extended_xlim[1], extended_xlim[2]), , drop = FALSE]
                if(nrow(sub_gr)) {
                    seekViewport(name = vp)
                    panel.fun(sub_gr)
                }
            }
            
        }

        # go to the highest vp
        seekViewport(name = qq("global_layout_@{i_plot}"))
        upViewport()
    }
}

# == title
# Get meta data in a cell
#
# == param
# -name name of the supported meta data, see 'details' section.
# -category which category. By default it is the current category.
# -track which track. By default it is the current track.
#
# == detail
# Following meta data can be retrieved:
#
# -name  name of the category.
# -xlim  xlim without including padding. Cells in the same column share the same ``xlim``.
# -ylim  ylim without including padding.
# -extended_xlim xlim with padding.
# -extended_ylim ylim with padding.
# -original_xlim xlim in original data.
# -original_ylim ylim in original data.
# -column which column in the layout.
# -row which row in the layout.
# -track which track in the layout.
#
# The vignette has a graphical explanation of all these meta data.
#
# == value
# Corresponding meta data that user queried.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
get_cell_meta_data = function(name, category, track) {

    if(missing(category)) category = .GENOMIC_LAYOUT$current_fa
    if(missing(track)) track = .GENOMIC_LAYOUT$current_track

    column = which(.GENOMIC_LAYOUT$fa == category) %% .GENOMIC_LAYOUT$ncol
    if(column == 0) column = .GENOMIC_LAYOUT$ncol
    
    switch(name,
           name = category,
           xlim = .GENOMIC_LAYOUT$xlim[column, ],
           ylim = .GENOMIC_LAYOUT$ylim[track, ],
           extended_xlim = .GENOMIC_LAYOUT$extended_xlim[column, ],
           extended_ylim = .GENOMIC_LAYOUT$extended_ylim[track, ],
           original_xlim = .GENOMIC_LAYOUT$original_xlim[category, ],
           original_ylim = .GENOMIC_LAYOUT$original_ylim[track, ],
           column = column,
           row = ceiling(which(.GENOMIC_LAYOUT$fa == category) / (.GENOMIC_LAYOUT$ncol+0.5)),
           track = track)
}

# == title
# Show index on each cell
#
# == detail
# The function adds name and index of track for each cell. 
# It is only for demonstration purpose.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
gtrellis_show_index = function() {

    op = qq.options(READ.ONLY = FALSE)
    on.exit(qq.options(op))
    qq.options(code.pattern = "@\\{CODE\\}")

    track = .GENOMIC_LAYOUT$n_track
    for(i in seq_len(track)) {
        add_track(track = i, panel.fun = function(gr) {
            nm = get_cell_meta_data("name")
            grid.text(qq("@{nm}\ntrack:@{i}"), unit(0.5, "npc"), unit(0.5, "npc"))
        })
    }
}

re_calculate_xlim = function(xlim, nrow, ncol, equal_width = FALSE) {
    xlim_start = rep(Inf, ncol)
    xlim_end = rep(-Inf, ncol)
    for(i in seq_len(nrow)) {
        for(j in seq_len(ncol)) {
            xlim_start[j] = ifelse(xlim_start[j] > xlim[j + (i-1)*ncol, 1], xlim[j + (i-1)*ncol, 1], xlim_start[j])
            xlim_end[j] = ifelse(xlim_end[j] < xlim[j + (i-1)*ncol, 2], xlim[j + (i-1)*ncol, 2], xlim_end[j])
                
        }
    }
    if(equal_width) {
        xlim_start = rep(min(xlim_start), length(xlim_start))
        xlim_end = rep(max(xlim_end), length(xlim_end))
    }
    return(cbind(xlim_start, xlim_end))
}

# fa is by row
# xlim[1] in the same column should be the same
check_xlim = function(xlim, nrow, ncol) {
    xlim = xlim[!grepl(".invisible", rownames(xlim)), , drop = FALSE]
    n = nrow(xlim)
    all(sapply(seq_len(ncol), function(column) {
        if(column == ncol) column = 0
        i = seq_len(n)[seq_len(n) %% ncol == column]
        almost_equal(xlim[i, 1])
    }))
}

almost_equal = function(x) {
    max(x) - min(x) < 1e-10
}

is_on_left = function(track, row, column, nrow, ncol, ntrack, track_show = rep(TRUE, ntrack)) {
    if(column != 1) return(FALSE)
    if(!track_show[track]) return(FALSE)
    track = length(which(track_show[1:track]))
    ntrack = sum(track_show)
    if((track + (row - 1)*ntrack) %% 2 == 1) return(TRUE)
    return(FALSE)
}

is_on_right = function(track, row, column, nrow, ncol, ntrack, track_show = rep(TRUE, ntrack)) {
    if(column != ncol) return(FALSE)
    if(!track_show[track]) return(FALSE)
    track = length(which(track_show[1:track]))
    ntrack = sum(track_show)
    if((track+(row-1)*ntrack) %% 2 == 0) return(TRUE)
    return(FALSE)
}

is_on_top = function(track, row, column, nrow, ncol, ntrack) {
    if(track == 1 && row == 1) {
        if(column %% 2 == 0) return(TRUE)
    } else {
        return(FALSE)
    }
    return(FALSE)
}

is_on_bottom = function(track, row, column, nrow, ncol, ntrack) {
    if(track == ntrack && row == nrow) {
        if(column %% 2 == 1) return(TRUE)
    } else {
        return(FALSE)
    }
    return(FALSE)
}

is_visible = function(row, column) {
    fa = .GENOMIC_LAYOUT$fa
    ncol = .GENOMIC_LAYOUT$ncol
    !grepl("^\\.invisible_", fa[column + (row-1)*ncol])
}


is_intersected = function(start, end, lim_start, lim_end) {
    l = (lim_start >= start & lim_start <= end) |
        (lim_end >= start & lim_end <= end) | 
        (lim_start <= start & lim_end >= end)
    return(l)
}
