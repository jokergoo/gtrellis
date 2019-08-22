
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
#          UCSC ftp automatically. Short scaffolds will be removed if they have obvious different length as others. 
#          Non-normal chromosomes will also be detected and removed. Sometimes this detection is not always correct and
#          if you find chromosomes shown on the plot is not what you expect, set ``category`` manually.
#          The argument is passed to `circlize::read.chromInfo`.
# -nrow Number of rows in the layout.
# -ncol Number of columns in the layout.
# -n_track Number of tracks in each genomic category.
# -track_height height of tracks. It should be numeric which means the value is relative and will be scaled into percent, or a `grid::unit` object.
# -track_ylim ranges on y axes of tracks. The value can be a vector of length two which means all tracks share same
#             y ranges, or a matrix with two columns, or a vector of length ``2*n_track`` which will be coerced
#             into the two-column matrix by rows.
# -track_axis whether show y axes for tracks. The value is logical that can be either length one or number of tracks.
# -track_ylab labels for tracks on y axes. The value can be either length one or number of tracks.
# -ylab_rot value can only be 0 or 90.
# -title title of the plot.
# -xlab labels on x axes.
# -xaxis whether show x axes.
# -xaxis_bin bin size for x axes.
# -equal_width whether all columns in the layout have the same width. If ``TRUE``, short categories will be extended
#              according to the longest category.
# -compact For the catgories which are put in a same row, will they be put compactly without being aligned by columns.
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
# -legend a `grid::grob` or `ComplexHeatmap::Legends-class` object, or a list of them.
# -legend_side side of the legend
# -padding padding of the plot. Elements correspond to bottom, left, top, right paddings.
# -remove_chr_prefix if chromosome names start with 'chr', whether to remove it.
#
# == detail
# Genome-level Trellis graph visualizes genomic data conditioned by genomic categories (e.g. chromosomes).
# For each genomic category, multiple dimensional data which are represented as tracks describe different features from different
# aspects. The `gtrellis_layout` function arranges genomic categories on the plot in a quite flexible way. Then users
# apply `add_track` to add self-defined graphics to the plot track by track.
#
# For more detailed demonstration of the function, please refer to the vignette.
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
    track_axis = TRUE, track_ylab = "", ylab_rot = 90, title = NULL, 
    xlab = "Genomic positions", xaxis = TRUE, xaxis_bin = NULL,
    equal_width = FALSE, compact = FALSE, border = TRUE, asist_ticks = TRUE,
    xpadding = c(0, 0), ypadding = c(0, 0), gap = unit(1, "mm"),
    byrow = TRUE, newpage = TRUE, add_name_track = FALSE, 
    name_fontsize = 10, name_track_fill = "#EEEEEE",
    add_ideogram_track = FALSE, ideogram_track_height = unit(2, "mm"), 
    axis_label_fontsize = 6, lab_fontsize = 10, title_fontsize = 16,
    legend = list(), legend_side = c("right", "bottom"),
    padding = unit(c(2, 2, 2, 2), "mm"), remove_chr_prefix = FALSE) {

    increase_plot_index()
    i_plot = get_plot_index()

    op = qq.options(READ.ONLY = FALSE)
    on.exit(qq.options(op))
    qq.options(code.pattern = "@\\{CODE\\}")

    legend_side = match.arg(legend_side)[1]

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
            
            chr_names = chromInfo$chromosome
            # to remove something like chr1_xxxxxx
            l = sapply(chr_names, function(nm) {
            	any(grepl(paste0("^", nm, "_"), chr_names))
            }) | !grepl("_", chr_names)
            chromosome = chromInfo$chromosome[l]

            chr_len = sort(chromInfo$chr.len[l], decreasing = TRUE)

            # sometimes there are small scaffold
            i = which(chr_len[seq_len(length(chr_len)-1)] / chr_len[seq_len(length(chr_len)-1)+1] > 50)
            if(length(i)) {
                i = i[1]
                chromosome = chromosome[chromosome %in% names(chr_len[chr_len >= chr_len[i]])]
            }

            category = chromosome
            if(length(category) == 0) {
                stop("Cannot identify any category, maybe you need to specify `category` manually.")
            }
        }
        chromInfo = read.chromInfo(species = species, chromosome.index = category)
        df = chromInfo$df
        category = chromInfo$chromosome
  
        xlim = as.matrix(df[2:3])
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
        xlim = as.matrix(xlim)
    }

    if(remove_chr_prefix) {
        fa = gsub("^chr", "", fa, ignore.case = TRUE)
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
    if(compact) {
        if(is.null(nrow)) {
            stop("`nrow` must be set if `compact` is TRUE.")
        }
        p = partition(xlim[, 2] - xlim[, 1], nrow)
        nrow = max(p)
        p = as.list(tapply(p, p, function(i) which(p == unique(i)))[as.character(seq_len(nrow))])
        ncol = sapply(p, length)
        byrow = TRUE
        equal_width = FALSE
    } else {
        if(is.null(nrow) && is.null(ncol)) nrow = 1
        if(!is.null(nrow) && is.null(ncol)) {
            ncol = ceiling(n/nrow)
        } else if(is.null(nrow) && !is.null(ncol)) {
            nrow = ceiling(n/ncol)
        }

        if(byrow) {
            foo = rep(seq_len(nrow), each = ncol)[1:n]
            nrow = max(foo)
            foo = factor(foo, levels = as.character(1:nrow))
            p = split(seq_len(n), foo)
        } else {
            foo = rep(seq_len(nrow), times = ncol)[1:n]
            nrow = max(foo)
            foo = factor(foo, levels = as.character(1:nrow))
            p = split(seq_len(n), foo)
        }
        ncol = sapply(p, length)
    }

    rownames(xlim) = fa
    
    # check whether the start are same for all catgories in a same column
    if(!compact) {
        for(i in seq_along(max(ncol))) {
            ind = sapply(p, function(x) x[i])
            ind = ind[!is.na(ind)]
            if(!almost_equal(xlim[ind, 1])) {
                stop("Start base in a same column should be the same.")
            }
        }
    }
    
    .GENOMIC_LAYOUT$fa = fa
    .GENOMIC_LAYOUT$nrow = nrow
    .GENOMIC_LAYOUT$ncol = ncol
    .GENOMIC_LAYOUT$n_track = n_track
    .GENOMIC_LAYOUT$track_axis_show = track_axis
    .GENOMIC_LAYOUT$current_fa = NULL
    .GENOMIC_LAYOUT$current_track = 0
    .GENOMIC_LAYOUT$p = p  # partitioning
    
    if(compact) {
        xlim2 = xlim
    } else {
        xlim2 = re_calculate_xlim(xlim, p, equal_width)
    }

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
    
    
    if(any(sapply(seq_len(ncol[1]), function(x) is_on_top(1, 1, x, nrow, ncol[1], n_track)))) {
        xaxis_top_height = grobHeight(textGrob("B", gp = gpar(axis_label_fontsize))) + axis_tick_height + axis_label_gap
    } else {
        xaxis_top_height = unit(0, "null")
    }
    if(any(sapply(seq_len(ncol[length(ncol)]), function(x) is_on_bottom(n_track, nrow, x, nrow, ncol[length(ncol)], n_track)))) {
        xaxis_bottom_height = grobHeight(textGrob("B", gp = gpar(axis_label_fontsize))) + axis_tick_height + axis_label_gap
    } else {
        xaxis_bottom_height = unit(0, "null")
    }
    if(xlab == "") {
        xlabel_height = unit(2, "mm")
    } else {
        xlabel_height = grobHeight(textGrob(xlab, gp = gpar(lab_fontsize))) + grobHeight(textGrob("foo", gp = gpar(lab_fontsize)))
    }

    # which tracks will have anno on left
    lstr = ""
    lstr_ylab = ""
    lstr_width = unit(0, "mm")
    lstr_ylab_height = unit(0, "mm")
    for(i in seq_len(nrow)) {
        for(k in seq_len(n_track)) {
            if(is_on_left(k, i, 1, nrow, ncol[i], n_track, track_axis | track_ylab != "")) {
                if(track_axis[k]) {
                    range = track_ylim[k, ]
                    axis_label= as.character(grid.pretty(range))
                    for(ax in axis_label) {
                        lstr_width = max(unit.c(lstr_width, grobWidth(textGrob(ax, gp = gpar(fontsize = axis_label_fontsize)))))
                    }
                    if(any(axis_label != "")) lstr = "a"
                }
                lstr_ylab_height = max(unit.c(lstr_ylab_height, grobWidth(textGrob(track_ylab[k], rot = ylab_rot, gp = gpar(fontsize = lab_fontsize)))))
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
        ylabel_left_width = lstr_ylab_height + grobHeight(textGrob("foo", gp = gpar(fontsize = lab_fontsize)))
    }

    lstr = ""
    lstr_ylab = ""
    lstr_width = unit(0, "mm")
    lstr_ylab_height = unit(0, "mm")
    for(i in seq_len(nrow)) {
        for(k in seq_len(n_track)) {
            if(is_on_right(k, i, ncol[i], nrow, ifelse(compact, ncol[i], ncol), n_track, track_axis | track_ylab != "")) {
                if(track_axis[k]) {
                    range = track_ylim[k, ]
                    axis_label= as.character(grid.pretty(range))
                    for(ax in axis_label) {
                        lstr_width = max(unit.c(lstr_width, grobWidth(textGrob(ax, gp = gpar(fontsize = axis_label_fontsize)))))
                    }
                    if(any(axis_label != "")) lstr = "a"
                }
                lstr_ylab_height = max(unit.c(lstr_ylab_height, grobWidth(textGrob(track_ylab[k], rot = ylab_rot, gp = gpar(fontsize = lab_fontsize)))))
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
        ylabel_right_width = lstr_ylab_height + grobHeight(textGrob("foo", gp = gpar(fontsize = lab_fontsize)))
    }

    if(title == "") {
        title_height = unit(2, "mm")
    } else {
        title_height = grobHeight(textGrob(title, gp = gpar(fontsize = title_fontsize))) + 0.5*grobHeight(textGrob("foo", gp = gpar(fontsize = title_fontsize)))
    }

    if(inherits(legend, "Legends")) legend = legend@grob
    if(inherits(legend, "grob")) legend = list(legend)
    legend = lapply(legend, function(x) {
        if(inherits(x, "Legends")) {
            x = x@grob
        } else {
            x
        }
    })
    legend_right_width = unit(0, "mm")
    legend_bottom_height = unit(0, "mm")
    if(length(legend) > 0) {
        if(legend_side == "right") {
            legend_right_width = max(do.call("unit.c", lapply(legend, grobWidth))) + unit(4, "mm")
        } else if(legend_side == "bottom") {
            legend_bottom_height = max(do.call("unit.c", lapply(legend, grobHeight))) + unit(4, "mm")
        }
    }

    if(length(padding) == 1) {
        padding = rep(padding, 4)
    } else if(length(padding) == 2) {
        padding = rep(padding, 2)
    } else if(length(padding) != 4) {
        stop("`padding` can only have length of 1, 2, 4")
    }

    if(newpage) grid.newpage(recording = FALSE)
    layout = grid.layout(nrow = 6, ncol = 6, widths = unit.c(ylabel_left_width, yaxis_left_width, unit(1, "null"), yaxis_right_width, ylabel_right_width, legend_right_width),
                                             heights = unit.c(title_height, xaxis_top_height, unit(1, "null"), xaxis_bottom_height, xlabel_height, legend_bottom_height))
    pushViewport(viewport(layout = layout, name = qq("global_layout_@{i_plot}"),
        x = padding[2], y = padding[1], width = unit(1, "npc") - padding[2] - padding[4],
        height = unit(1, "npc") - padding[1] - padding[3], just = c("left", "bottom")))
    
    if(!is.null(title)) {
        pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 3))
        grid.text(title, gp = gpar(fontsize = title_fontsize))
        upViewport()
    }
    
    if(!is.null(xlab)) {
        pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 3))
        grid.text(xlab, gp = gpar(fontsize = lab_fontsize))
        upViewport()
    }

    if(length(legend) > 0) {
        if(legend_side == "right") {
            pushViewport(viewport(x = unit(2, "mm"), just = "left", layout.pos.row = 3, layout.pos.col = 6))
            gap = unit(2, "mm")
            # draw the list of legend
            legend_height = sum(do.call("unit.c", lapply(legend, grobHeight))) + gap*(length(legend)-1)
            y = unit(0.5, "npc") + legend_height*0.5 
            for(i in seq_along(legend)) {
                pushViewport(viewport(x = unit(2, "mm"), y = y, height = grobHeight(legend[[i]]), width = grobWidth(legend[[i]]), just = c("left", "top")))
                grid.draw(legend[[i]])
                upViewport()
                y = y - gap - grobHeight(legend[[i]])
            }
            upViewport()
        } else if(legend_side == "bottom") {
            pushViewport(viewport(y = unit(2, "mm"), just = "bottom", layout.pos.row = 6, layout.pos.col = 3))
            gap = unit(2, "mm")
            # draw the list of legend
            legend_width = sum(do.call("unit.c", lapply(legend, grobWidth))) + gap*(length(legend)-1)
            x = unit(0.5, "npc") - legend_width*0.5 
            for(i in seq_along(legend)) {
                pushViewport(viewport(y = unit(2, "mm"), x = x, height = grobHeight(legend[[i]]), width = grobWidth(legend[[i]]), just = c("left", "bottom")))
                grid.draw(legend[[i]])
                upViewport()
                x = x + gap + grobWidth(legend[[i]])
            }
            upViewport() 
        }

        
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
    
    
    chr_height = unit(1, "npc")*(1/nrow) - (nrow - 1)*ygap*(1/nrow)
    chr_y = (nrow:1 - 1)*ygap + (nrow:1 - 0.5) * chr_height

    # arrange fas on the plot
    for(i in seq_len(nrow)) {
        current_ind = p[[i]]
        if(equal_width) {
            chr_width = unit(1, "npc")*(1/max(ncol)) - (max(ncol) - 1)*xgap*(1/max(ncol))
            chr_x = (1:ncol[i] - 1)*xgap + (1:ncol[i] - 0.5) * chr_width
            chr_width = rep(chr_width, ncol[i])
        } else {
            max_sum = max(sapply(p, function(i) sum(extended_xlim[i, 2] - extended_xlim[i, 1])))
            ratio = (extended_xlim[current_ind, 2] - extended_xlim[current_ind, 1])/ max_sum
            chr_width = unit(1, "npc")*ratio - (ifelse(compact, ncol[i], ncol) - 1)*xgap*ratio
            chr_x = NULL
            for(k in seq_along(chr_width)) {
                if(k == 1) {
                    chr_x[[k]] = chr_width[k]*0.5
                } else {
                    chr_x[[k]] = (k - 1)*xgap + sum(chr_width[seq_len(k-1)]) + chr_width[k]*0.5
                }
            }
            chr_x = do.call("unit.c", chr_x)
        }
        
        for(j in seq_len(ncol[i])) {
            pushViewport(viewport(x = chr_x[j], y = chr_y[i], width = chr_width[j], height = chr_height, name = qq("@{fa[current_ind[j]]}_container_@{i_plot}")))
            if(border) grid.rect(gp = gpar(col = "black", fill = "transparent"))
            upViewport()
        }
    }
    upViewport()

    # add tracks
    for(i in seq_len(nrow)) {
        current_ind = p[[i]]
        for(j in seq_along(current_ind)) {
            seekViewport(name = qq("@{fa[current_ind[j]]}_container_@{i_plot}"))
            layout = grid.layout(nrow = n_track, ncol = 1, heights = track_height)
            pushViewport(viewport(layout = layout, name = qq("@{fa[current_ind[j]]}_layout_@{i_plot}")))
            for(k in seq_len(n_track)) {
                
                pushViewport(viewport(layout.pos.col = 1, layout.pos.row = k, name = qq("@{fa[current_ind[j]]}_track_@{k}_@{i_plot}")))
                pushViewport(plotViewport(margins = c(0, 0, 0, 0), name = qq("@{fa[current_ind[j]]}_track_@{k}_plotvp_@{i_plot}")))
                pushViewport(dataViewport(xscale = extended_xlim[current_ind[j], ], yscale = extended_track_ylim[k, ], extension = 0, name = qq("@{fa[current_ind[j]]}_track_@{k}_datavp_@{i_plot}")))
                pushViewport(dataViewport(xscale = extended_xlim[current_ind[j], ], yscale = extended_track_ylim[k, ], extension = 0, clip = TRUE, name = qq("@{fa[current_ind[j]]}_track_@{k}_datavp_clip_@{i_plot}")))
                if(border && k > 1) grid.lines(c(0, 1), c(1, 1))
                u = convertWidth(unit(1, "mm"), "native", valueOnly = TRUE)
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
            if(is_on_left(k, i, 1, nrow, ncol[i], n_track, track_axis | track_ylab != "")) {
                if(track_ylab[k] != "") {
                    grid.text(track_ylab[k], rot = ylab_rot, gp = gpar(fontsize = lab_fontsize), just = ifelse(ylab_rot == 90, "bottom", "right"))
                }
            }
            upViewport()
        }
        upViewport()

        upViewport()
    }
    upViewport()

    # y-labels on the right
    if(!compact) {
        pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 5))
        for(i in seq_len(nrow)) {
            pushViewport(viewport(y = chr_y[i], height = chr_height, name = qq("ylab_row_@{i}_right_@{i_plot}")))
            
            layout = grid.layout(nrow = n_track, ncol = 1, heights = track_height)
            pushViewport(viewport(layout = layout))
            for(k in seq_len(n_track)) {
                    
                pushViewport(viewport(layout.pos.col = 1, layout.pos.row = k, name = qq("ylab_row_@{i}_right_track_@{k}_@{i_plot}")))
                if(is_on_right(k, i, ncol[i], nrow, ncol[i], n_track, track_axis | track_ylab != "") && ncol[i] == max(ncol)) {
                    if(track_ylab[k] != "") {
                        grid.text(track_ylab[k], rot = ylab_rot, gp = gpar(fontsize = lab_fontsize), just = ifelse(ylab_rot == 90, "top", "left"))
                    }
                }
                upViewport()
            }
            upViewport()

            upViewport()
        }
        upViewport()
    }
    
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
        current_ind = p[[1]]
        for(j in seq_len(ncol[1])) {
            seekViewport(name = qq("@{fa[current_ind[j]]}_track_1_datavp_@{i_plot}"))
            if(is.null(xaxis_bin)) {
                xbreaks = seq(grid.pretty(xlim2[current_ind[j], ])[1], xlim2[current_ind[j], 2], by = breaks[2]-breaks[1])
            } else {
                xbreaks = seq(grid.pretty(xlim2[current_ind[j], ])[1], xlim2[current_ind[j], 2], by = xaxis_bin)
            }
            if(length(xbreaks) == 0) xbreaks = grid.pretty(xlim2[current_ind[j], 1])[1]
            #xbreaks = grid.pretty(xlim2[j, ])
                
            if(is_on_top(1, i, j, nrow, ncol[1], n_track)) {
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

        # x-axis on bottom
        i = nrow
        current_ind = p[[nrow]]
        for(j in seq_len(ncol[nrow])) {
            seekViewport(name = qq("@{fa[current_ind[j]]}_track_@{n_track}_datavp_@{i_plot}"))
            if(is.null(xaxis_bin)) {
                xbreaks = seq(grid.pretty(xlim2[current_ind[j], ])[1], xlim2[current_ind[j], 2], by = breaks[2]-breaks[1])
            } else {
                xbreaks = seq(grid.pretty(xlim2[current_ind[j], ])[1], xlim2[current_ind[j], 2], by = xaxis_bin)
            }
            #xbreaks = grid.pretty(xlim2[j, ])
            
            if(is_on_bottom(n_track, i, j, nrow, max(ncol), n_track)) {
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
        }
        if(asist_ticks && nrow > 1) {
            for(i in seq(nrow-1, 1)) {
                current_ind = p[[i]]
                for(j in seq(ncol[i], 1)) {
                    total_width = sum(extended_xlim[current_ind[1:j], 2] - extended_xlim[current_ind[1:j], 1])
                    total_width_short = sum(extended_xlim[p[[i+1]], 2] - extended_xlim[p[[i+1]], 1])
                    last_width = (extended_xlim[current_ind[j], 2] - extended_xlim[current_ind[j], 1])
                    total_width = total_width + convertWidth(gap[2]*(j-1), "mm", valueOnly = TRUE)*u
                    total_width_short = total_width_short + convertWidth(gap[2]*(length(p[[i+1]])-1), "mm", valueOnly = TRUE)*u
                    if(total_width - total_width_short > last_width) {
                        if(is.null(xaxis_bin)) {
                            xbreaks = seq(grid.pretty(xlim2[current_ind[j], ])[1], xlim2[current_ind[j], 2], by = breaks[2]-breaks[1])
                        } else {
                            xbreaks = seq(grid.pretty(xlim2[current_ind[j], ])[1], xlim2[current_ind[j], 2], by = xaxis_bin)
                        }
                        seekViewport(name = qq("@{fa[current_ind[j]]}_track_@{n_track}_datavp_@{i_plot}"))
                        grid.segments(xbreaks, unit(0, "npc") - axis_tick_height,
                                      xbreaks, unit(0, "npc"), default.units = "native")
                    }
                }
            }       
        }
    }

    # y-axis on left
    j = 1
    for(i in seq_len(nrow)) {
        current_ind = p[[i]]
        for(k in seq_len(n_track)) {
            seekViewport(name = qq("@{fa[current_ind[j]]}_track_@{k}_datavp_@{i_plot}"))
                ybreaks = grid.pretty(.GENOMIC_LAYOUT$ylim[k, ])
            

            if(is_on_left(k, i, j, nrow, ncol[i], n_track, track_axis | track_ylab != "")) {
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

    # y-axis on right
    for(i in seq_len(nrow)) {
        current_ind = p[[i]]
        j = ncol[i]
        for(k in seq_len(n_track)) {
            seekViewport(name = qq("@{fa[current_ind[j]]}_track_@{k}_datavp_@{i_plot}"))
            ybreaks = grid.pretty(.GENOMIC_LAYOUT$ylim[k, ])
            

            if(is_on_right(k, i, j, nrow, ifelse(compact, ncol[i], max(ncol)), n_track, track_axis | track_ylab != "")) {
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
                if(compact) {
                    if(track_ylab[k] != "") {
                        grid.text(track_ylab[k], x = unit(1, "npc") + yaxis_right_width + ylabel_right_width*0.5, 
                            just = ifelse(ylab_rot == 90, "top", "left"), rot = ylab_rot, gp = gpar(fontsize = lab_fontsize))
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
        }
    }

    seekViewport(name = qq("global_layout_@{i_plot}"))
    upViewport()

    .GENOMIC_LAYOUT$current_fa = fa[1]
    .GENOMIC_LAYOUT$current_track = 0

    if(add_name_track) {
        add_track(panel_fun = function(gr){
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

# calculate the common xlim for categories in a same column
re_calculate_xlim = function(xlim, p, equal_width = FALSE) {
    ncol = sapply(p, length)
    xlim_start = numeric(sum(ncol))
    xlim_end = numeric(sum(ncol))
    for(i in seq_len(max(ncol))) {
        ind = sapply(p, function(x) {
            x[i]
        })
        ind = ind[!is.na(ind)]
        xlim_start[ind] = min(xlim[ind, 1])
        xlim_end[ind] = max(xlim[ind, 2])
    }
    if(equal_width) {
        xlim_start = rep(min(xlim_start), length(xlim_start))
        xlim_end = rep(max(xlim_end), length(xlim_end))
    }
    return(cbind(xlim_start, xlim_end))
}

almost_equal = function(x) {
    if(length(x) == 1) return(TRUE)
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
