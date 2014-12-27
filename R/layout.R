
# use en environment to put global environment
.GENOMIC_LAYOUT = new.env()

# == title
# initialize genomic trellis layout
#
# == param
# -data a data frame, same rule as ``data`` argument in `circlize::circos.genomicInitialize`.
# -chromosome chromosome
# -species species
# -nrow nrow of the layout
# -ncol ncol of the layout
# -track_number how many tracks
# -track_height height of tracks, should be numeric which means the value is relative
#               or `grid::unit` class
# -track_ylim track ylim, can be a vector of length two, or a vector of length 2*track_number or a matrix
# -track_axis track axis, whether show y-axis for a track
# -track_ylab track label, ``''`` means there is no label for the track
# -main title
# -xlab xlab
# -xaxis whether add x-axis
# -equal_width whether all columns hava the same width
# -border draw border
# -xpadding xpadding, numeric value means relative ratio to the cell width. use `base::I` to set it as absolute
#           value which is measured in the datavp.
# -ypadding ypadding, only numeric value
# -gap 0 or a `grid::unit` class. If it is length 2, the first one corresponds to the gaps between rows and
#      the seond corresponds to the gaps between columns
# -byrow arrange categories (e.g. chromosomes) by rows ?
# -newpage whether call `grid::grid.newpage` to initialize on a new graphic devide
# -add_name_track whether add a pre-defined name track (insert before the first track)
# -name_fontsize font size for cell names, the font size also controls the height of name track
# -name_track_fill filled color for name track
# -add_ideogram_track whether add a pre-defined ideogram track (insert after the last track).
# -ideogram_track_height height of ideogram track
# -axis_label_fontsize font size for axis labels
# -lab_fontsize font size for x-labels and y-labels
# -main_fontsize font size for title
#
# == detail
# please go to the vignette.
initialize_layout = function(data = NULL, chromosome = NULL, 
    species = NULL, nrow = NULL, ncol = NULL,
    track_number = 1, track_height =rep(1, track_number),
    track_ylim = do.call("rbind", rep(list(0:1), track_number)),
    track_axis = rep(TRUE, track_number), track_ylab = "", 
    main = NULL, xlab = "Position", xaxis = TRUE,
    equal_width = FALSE, border = TRUE,
    xpadding = c(0, 0), ypadding = c(0, 0), gap = unit(1, "mm"),
    byrow = TRUE, newpage = TRUE, add_name_track = FALSE, 
    name_fontsize = 10, name_track_fill = "#EEEEEE",
    add_ideogram_track = FALSE, ideogram_track_height = unit(2, "mm"), 
    axis_label_fontsize = 6, lab_fontsize = 10, main_fontsize = 16) {


    ## start from xlim
    if(is.null(data)) {
        cytoband = read.cytoband(species = species)
        df = cytoband$df
        if(is.null(chromosome)) chromosome = cytoband$chromosome
        chromosome = unique(chromosome)
        x1 = tapply(df[[2]], df[[1]], min)[chromosome]
        x2 = tapply(df[[3]], df[[1]], max)[chromosome]
        xlim = cbind(x1, x2)
        fa = chromosome
    } else {
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

    if(is.null(main)) main = ""
    if(is.na(main)) main = ""

    axis_tick_height = unit(1, "mm")
    axis_label_gap = unit(1, "mm")

    # check track*
    if(is.unit(track_height)) {
        if(length(track_height) != track_number) {
            stop(qq("Length of `track_height` should be @{track_number} if it is a `unit`\n"))
        }
    } else {
        if(length(track_height) == 1) {
            track_height = rep(1, track_number)
        } else if(length(track_height) != track_number) {
            stop(qq("Length of `track_height` should be either 1 or @{track_number}\n"))
        }
        if(add_name_track || add_ideogram_track) {
            track_height = unit(track_height/sum(track_height), "null")
        } else {
            track_height = unit(track_height/sum(track_height), "npc")
        }
    }
    if(any(inherits(track_ylim, c("matrix", "data.frame")))) {
        if(nrow(track_ylim) == 1) {
            track_ylim = do.call("rbind", rep(list(c(track_ylim[1, 1], track_ylim[1, 2])), track_number))

        } else if(nrow(track_ylim) != track_number) {
            stop(qq("nrow of `track_ylim` should be either 1 or @{track_number}\n"))
        }
    } else {
        if(length(track_ylim) == 2) {
            track_ylim = do.call("rbind", rep(list(track_ylim[1:2]), track_number))
        } else if(length(track_ylim) == 2*track_number) {
            track_ylim = matrix(track_ylim, ncol = 2, byrow = TRUE)
        } else {
            stop(qq("If `track_ylim` is atomic, the length should be either 2 or @{2*track_number}\n"))
        }
    }
    track_axis = as.logical(track_axis)
    if(length(track_axis) == 1) {
        track_axis = rep(track_axis, track_number)
    } else if(length(track_axis) != track_number) {
        stop(qq("Length of `track_axis` should be either 1 or @{track_number}\n"))
    }
    if(is.null(track_ylab)) track_ylab = ""
    if(length(track_ylab) == 1) {
        track_ylab = rep(track_ylab, track_number)
    } else if(length(track_ylab) != track_number) {
        stop(qq("Length of `track_ylab` should be either 1 or @{track_number}\n"))
    }
    track_ylab[is.na(track_ylab)] = ""

    if(add_name_track) {
        track_height = unit.c(1.5*max(grobHeight(textGrob(fa, gp = gpar(fontsize = name_fontsize)))), track_height)
        track_number = track_number + 1
        track_ylim = rbind(c(0, 1), track_ylim)
        track_axis = c(FALSE, track_axis)
        track_ylab = c("", track_ylab)
    }
    if(add_ideogram_track) {
        track_height = unit.c(track_height, ideogram_track_height)
        track_number = track_number + 1
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
    
    .GENOMIC_LAYOUT$fa = fa
    .GENOMIC_LAYOUT$nrow = nrow
    .GENOMIC_LAYOUT$ncol = ncol
    .GENOMIC_LAYOUT$track_number = track_number
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
    
    
    if(any(sapply(1:ncol, function(x) is_on_top(1, 1, x, nrow, ncol, track_number)))) {
        xaxis_top_height = grobHeight(textGrob("B", gp = gpar(axis_label_fontsize))) + axis_tick_height + axis_label_gap
    } else {
        xaxis_top_height = unit(0, "null")
    }
    if(any(sapply(1:ncol, function(x) is_on_bottom(track_number, nrow, x, nrow, ncol, track_number)))) {
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
        for(k in seq_len(track_number)) {
            if(is_on_left(k, i, 1, nrow, ncol, track_number, track_axis | track_ylab != "")) {
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
        for(k in seq_len(track_number)) {
            if(is_on_right(k, i, ncol, nrow, ncol, track_number, track_axis | track_ylab != "")) {
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

    if(main == "") {
        main_height = unit(2, "mm")
    } else {
        main_height = grobHeight(textGrob(main, gp = gpar(fontface = "bold", fontsize = main_fontsize)))*1.5
    }

    if(newpage) grid.newpage(recording = FALSE)
    layout = grid.layout(nrow = 5, ncol = 5, widths = unit.c(ylabel_left_width, yaxis_left_width, unit(1, "null"), yaxis_right_width, ylabel_right_width),
                                             heights = unit.c(main_height, xaxis_top_height, unit(1, "null"), xaxis_bottom_height, xlabel_height))
    pushViewport(viewport(layout = layout, name = "global_layout"))
    
    if(!is.null(main)) {
        pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 3))
        grid.text(main, gp = gpar(fontface = "bold", fontsize = main_fontsize))
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
    pushViewport(viewport(layout.pos.col = 3, layout.pos.row = 3, name = "main_container"))
    
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
            pushViewport(viewport(x = chr_x[j], y = chr_y[i], width = chr_width[j], height = chr_height, name = qq("@{fa[j + (i-1)*ncol]}_container")))
            if(is_visible(i, j) && border) grid.rect()
            upViewport()
        }
    }
    upViewport()
    
    # add tracks
    for(i in seq_len(nrow)) {
        for(j in seq_len(ncol)) {
            seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_container"))
            layout = grid.layout(nrow = track_number, ncol = 1, heights = track_height)
            pushViewport(viewport(layout = layout, name = qq("@{fa[j + (i-1)*ncol]}_layout")))
            for(k in seq_len(track_number)) {
                
                pushViewport(viewport(layout.pos.col = 1, layout.pos.row = k, name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}")))
                pushViewport(plotViewport(margins = c(0, 0, 0, 0), name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_plotvp")))
                pushViewport(dataViewport(xscale = extended_xlim[j, ], yscale = extended_track_ylim[k, ], extension = 0, name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_datavp")))
                pushViewport(dataViewport(xscale = extended_xlim[j, ], yscale = extended_track_ylim[k, ], extension = 0, clip = TRUE, name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_datavp_clip")))
                if(is_visible(i, j) && border && k > 1) grid.lines(c(0, 1), c(1, 1))
                upViewport(4)
            }
            upViewport()
        }
    }
    seekViewport(name = "global_layout")

    # y-labels on the left
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
    for(i in seq_len(nrow)) {
        pushViewport(viewport(y = chr_y[i], height = chr_height, name = qq("ylab_row_@{i}_left")))
        
        layout = grid.layout(nrow = track_number, ncol = 1, heights = track_height)
        pushViewport(viewport(layout = layout))
        for(k in seq_len(track_number)) {
                
            pushViewport(viewport(layout.pos.col = 1, layout.pos.row = k, name = qq("ylab_row_@{i}_left_track_@{k}")))
            if(is_on_left(k, i, 1, nrow, ncol, track_number, track_axis | track_ylab != "") && is_visible(i, 1)) {
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
        pushViewport(viewport(y = chr_y[i], height = chr_height, name = qq("ylab_row_@{i}_right")))
        
        layout = grid.layout(nrow = track_number, ncol = 1, heights = track_height)
        pushViewport(viewport(layout = layout))
        for(k in seq_len(track_number)) {
                
            pushViewport(viewport(layout.pos.col = 1, layout.pos.row = k, name = qq("ylab_row_@{i}_right_track_@{k}")))
            if(is_on_right(k, i, ncol, nrow, ncol, track_number, track_axis | track_ylab != "") && is_visible(i, ncol)) {
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
            paste0(x/1e3, "MB")
        } else {
            paste0(x, "B")
        }
    }

    # x-axis on top
    if(xaxis) {
        il = which.max(xlim2[, 2] - xlim2[, 1])[1]
        breaks = grid.pretty(c(xlim2[il, 1], xlim2[il, 2]))
        i = 1
        for(j in seq_len(ncol)) {
            if(is_visible(i, j)) {
                seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_track_1_datavp"))
                xbreaks = seq(grid.pretty(xlim2[j, ])[1], xlim2[j, 2], by = breaks[2]-breaks[1])
                if(length(xbreaks) == 0) xbreaks = grid.pretty(xlim2[j, 1])[1]
                #xbreaks = grid.pretty(xlim2[j, ])
                grid.segments(xbreaks, unit(1, "npc") + axis_tick_height,
                              xbreaks, unit(1, "npc"), default.units = "native")
                    
                if(is_on_top(1, i, j, nrow, ncol, track_number)) {
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
                }
            }
        }
        
        # x-axis on bottom
        i = nrow
        for(j in seq_len(ncol)) {
            if(is_visible(i, j)) {
                seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_track_@{track_number}_datavp"))
                xbreaks = seq(grid.pretty(xlim2[j, ])[1], xlim2[j, 2], by = breaks[2]-breaks[1])
                #xbreaks = grid.pretty(xlim2[j, ])
                grid.segments(xbreaks, unit(0, "npc") - axis_tick_height,
                              xbreaks, unit(0, "npc"), default.units = "native")
                if(is_on_bottom(track_number, i, j, nrow, ncol, track_number)) {
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
                }
            } else {
                while(1) {
                    i2 = i - 1
                    if(is_visible(i2, j) || i2 == 1) {
                        break
                    }
                }
                seekViewport(name = qq("@{fa[j + (i2-1)*ncol]}_track_@{track_number}_datavp"))
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
        for(k in seq_len(track_number)) {
            if(is_visible(i, j)) {
                seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_datavp"))
                    ybreaks = grid.pretty(.GENOMIC_LAYOUT$ylim[k, ])
                if(track_axis[k]) {
                    grid.segments(unit(0, "npc") - axis_tick_height, ybreaks,
                                  unit(0, "npc"), ybreaks, default.units = "native")
                }

                if(is_on_left(k, i, j, nrow, ncol, track_number, track_axis | track_ylab != "")) {
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
                }
            }
        }
    }
    
    # y-axis on right
    j = ncol
    for(i in seq_len(nrow)) {
        for(k in seq_len(track_number)) {
            if(is_visible(i, j)) {
                seekViewport(name = qq("@{fa[j + (i-1)*ncol]}_track_@{k}_datavp"))
                ybreaks = grid.pretty(.GENOMIC_LAYOUT$ylim[k, ])
                if(track_axis[k]) {
                    grid.segments(unit(1, "npc") + axis_tick_height, ybreaks,
                                  unit(1, "npc"), ybreaks, default.units = "native")
                }

                if(is_on_right(k, i, j, nrow, ncol, track_number, track_axis | track_ylab != "")) {
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
                }
            } else {
                while(1) {
                    j2 = j - 1
                    if(is_visible(i, j2) || j2 == 1) {
                        break
                    }
                }
                seekViewport(name = qq("@{fa[j2 + (i-1)*ncol]}_track_@{k}_datavp"))
                ybreaks = grid.pretty(.GENOMIC_LAYOUT$ylim[k, ])
                if(track_axis[k]) {
                    grid.segments(unit(1, "npc") + axis_tick_height, ybreaks,
                                  unit(1, "npc"), ybreaks, default.units = "native")
                }
            }
        }
    }
    
    seekViewport(name = "global_layout")
    upViewport()

    .GENOMIC_LAYOUT$current_fa = fa[1]
    .GENOMIC_LAYOUT$current_track = 0

    if(add_name_track) {
        add_track(panel.fun = function(gr){
            nm = get_current_cell_meta_data("name")
            grid.rect(gp = gpar(fill = name_track_fill, col = "#000000"))
            grid.text(nm, gp = gpar(fontsize = name_fontsize))
        })
        .GENOMIC_LAYOUT$current_track = 1
    }

    if(add_ideogram_track) {
        current_track = .GENOMIC_LAYOUT$current_track
        add_ideogram_track(species, track_number)
        .GENOMIC_LAYOUT$current_track = current_track
    }
    
}

# == title
# add ideogram track
#
# == param
# -species species
# -i_track which track
#
# == detail
# add an ideogram track
add_ideogram_track = function(species = NULL, i_track = get_current_cell_meta_data("i_track")+1) {

	cytoband = read.cytoband(species = species)
    cytoband_df = cytoband$df
    add_track(cytoband_df, i_track = i_track, clip = TRUE, panel.fun = function(gr) {
        cytoband_chr = gr
        grid.rect( cytoband_chr[[2]], unit(0, "npc"),
                   width = cytoband_chr[[3]] - cytoband_chr[[2]], height = unit(1, "npc"),
                   default.units = "native", hjust = 0, vjust = 0,
                   gp = gpar(fill = cytoband.col(cytoband_chr[[5]])) )
        grid.rect(min(cytoband_chr[[2]]), unit(0, "npc"),
                  width = max(cytoband_chr[[3]]) - min(cytoband_chr[[2]]), height = unit(1, "npc"),
                  default.units = "native", hjust = 0, vjust = 0)
    })
}

# == title
# add graphics by track
#
# == param
# -gr a data frame in BED format or a ``GRanges`` object.
# -cate categories (e.g. chromosome)
# -i_track which track, by default it is the next track
# -clip whether graphics are restricted inside the cell
# -panel.fun panel function to add graphics on each 'cell'
#
# == detail
# As same as ``panel.fun`` in ``circlize`` package, ``panel.fun``
# will be applied on every cell in the current track. ``gr`` in ``panel.fun``
# is a subset of the main ``gr`` which only contains data for the current
# category.
add_track = function(gr = NULL, cate = NULL, i_track = get_current_cell_meta_data("i_track")+1, 
    clip = TRUE, panel.fun = function(gr) NULL) {
    
    if(i_track > .GENOMIC_LAYOUT$track_number || i_track < 1) {
        stop(qq("`i_track` should be between [1, @{.GENOMIC_LAYOUT$track_number}]\n"))
    }

    all_fa = .GENOMIC_LAYOUT$fa
    fa = all_fa[ !grepl("^\\.invisible_", all_fa) ]

    if(is.null(cate)) {
        if(is.null(gr)) {
            fa = fa
        } else {
            if(inherits(gr, "GenomicRanges")) {
                fa = unique(seqnames(gr))
            } else {
                fa = unique(as.character(gr[[1]]))
            }
            fa = fa[fa %in% all_fa]
        }
    } else {
        fa = cate[cate %in% all_fa]
    }
    
    for(chr in fa) {
        .GENOMIC_LAYOUT$current_fa = chr
        .GENOMIC_LAYOUT$current_track = i_track
        
        if(clip) {
        	vp = qq("@{chr}_track_@{i_track}_datavp_clip")
        } else {
        	vp = qq("@{chr}_track_@{i_track}_datavp")
        }
        if(is.null(gr)) {
            seekViewport(name = vp)
            panel.fun(NULL)
        } else {
            extended_xlim = get_current_cell_meta_data("extended_xlim")
            if(inherits(gr, "GenomicRanges")) {
                sub_gr = gr[seqnames(gr) == chr]
                sub_gr = sub_gr[is_intersected(start(sub_gr), end(sub_gr), extended_xlim[1], extended_xlim[2])]
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
        seekViewport(name = "global_layout")
        upViewport()
    }
}

# == title
# get current cell meta data
#
# == param
# -name name
#
# == detail
# Following meta data can be retrieved:
#
# -name  name
# -xlim  xlim without including padding, cells in a same column shares the same ``xlim``
# -ylim  yiim without including padding
# -extended_xlim xlim with padding
# -extended_ylim ylim with padding
# -original_xlim xlim in original data
# -original_ylim ylim in original data
# -i_col which column in the layout
# -i_row which row in the layout
# -i_track which track in the layout
#
get_current_cell_meta_data = function(name) {
    i_col = which(.GENOMIC_LAYOUT$fa == .GENOMIC_LAYOUT$current_fa) %% .GENOMIC_LAYOUT$ncol
    if(i_col == 0) i_col = .GENOMIC_LAYOUT$ncol
    
    switch(name,
           name = .GENOMIC_LAYOUT$current_fa,
           xlim = .GENOMIC_LAYOUT$xlim[i_col, ],
           ylim = .GENOMIC_LAYOUT$ylim[.GENOMIC_LAYOUT$current_track, ],
           extended_xlim = .GENOMIC_LAYOUT$extended_xlim[i_col, ],
           extended_ylim = .GENOMIC_LAYOUT$extended_ylim[.GENOMIC_LAYOUT$current_track, ],
           original_xlim = .GENOMIC_LAYOUT$original_xlim[.GENOMIC_LAYOUT$current_fa, ],
           original_ylim = .GENOMIC_LAYOUT$original_ylim[.GENOMIC_LAYOUT$current_track, ],
           i_col = i_col,
           i_row = ceiling(which(.GENOMIC_LAYOUT$fa == .GENOMIC_LAYOUT$current_fa) / (.GENOMIC_LAYOUT$ncol+0.5)),
           i_track = .GENOMIC_LAYOUT$current_track)
}

# == title
# add annotation of each cell
#
# == detail
# add the names and the index of the track on each cell
add_cell_info = function() {
    track = .GENOMIC_LAYOUT$track_number
    for(i in seq_len(track)) {
        add_track(i_track = i, panel.fun = function(gr) {
            nm = get_current_cell_meta_data("name")
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

is_on_left = function(i_track, i_row, i_col, nrow, ncol, ntrack, track_show = rep(TRUE, ntrack)) {
    if(i_col != 1) return(FALSE)
    if(!track_show[i_track]) return(FALSE)
    i_track = length(which(track_show[1:i_track]))
    ntrack = sum(track_show)
    if((i_track + (i_row - 1)*ntrack) %% 2 == 1) return(TRUE)
    return(FALSE)
}

is_on_right = function(i_track, i_row, i_col, nrow, ncol, ntrack, track_show = rep(TRUE, ntrack)) {
    if(i_col != ncol) return(FALSE)
    if(!track_show[i_track]) return(FALSE)
    i_track = length(which(track_show[1:i_track]))
    ntrack = sum(track_show)
    if((i_track+(i_row-1)*ntrack) %% 2 == 0) return(TRUE)
    return(FALSE)
}

is_on_top = function(i_track, i_row, i_col, nrow, ncol, ntrack) {
    if(i_track == 1 && i_row == 1) {
        if(i_col %% 2 == 0) return(TRUE)
    } else {
        return(FALSE)
    }
    return(FALSE)
}

is_on_bottom = function(i_track, i_row, i_col, nrow, ncol, ntrack) {
    if(i_track == ntrack && i_row == nrow) {
        if(i_col %% 2 == 1) return(TRUE)
    } else {
        return(FALSE)
    }
    return(FALSE)
}

is_visible = function(i_row, i_col) {
    fa = .GENOMIC_LAYOUT$fa
    ncol = .GENOMIC_LAYOUT$ncol
    !grepl("^\\.invisible_", fa[i_col + (i_row-1)*ncol])
}


is_intersected = function(start, end, lim_start, lim_end) {
    l = (lim_start >= start & lim_start <= end) |
        (lim_end >= start & lim_end <= end) | 
        (lim_start <= start & lim_end >= end)
    return(l)
}