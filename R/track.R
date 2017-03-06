
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
add_ideogram_track = function(cytoband = system.file("extdata", "cytoBand.txt", 
    package = "circlize"), species = NULL, track = current_track() + 1) {

	cytoband = read.cytoband(species = species)
    cytoband_df = cytoband$df
    add_track(cytoband_df, track = track, clip = TRUE, panel_fun = function(gr) {
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
# -panel_fun self-defined panel function to add graphics in each 'cell'. THe argument ``gr`` in ``panel_fun`` 
#            only contains data for the current category which is a subset of the main ``gr``. The function can also
#            contains no argument if nothing needs to be passed in.
# -panel.fun deprecated
# -use_raster whether render the each panel as a raster image. It helps to reduce file size when the file size is huge.
# -raster_device graphic device which is used to generate the raster image
# -raster_quality a value set to larger than 1 will improve the quality of the raster image. A temporary image with
#       ``raster_quality``*``raster_quality`` times the original size of panel is generated first and then fit into
#        the panel by `grid::grid.raster`.
# -raster_device_param a list of further parameters for the selected graphic device
#
# == detail
# Initialization of the Trellis layout and adding graphics are two independent steps.
# Once the layout initialization finished, each cell will be an independent plotting region.
# As same as ``panel_fun`` in `circlize::circlize-package`, the self-defined function ``panel_fun``
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
# There are several functions which draw specific graphics and are implemented by `add_track`:
# 
# - `add_points_track`
# - `add_segments_track`
# - `add_lines_track`
# - `add_rect_track`
# - `add_heatmap_track`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
add_track = function(gr = NULL, category = NULL, track = current_track() + 1, 
    clip = TRUE, panel_fun = function(gr) NULL, panel.fun = NULL,
    use_raster = FALSE, 
    raster_device = c("png", "jpeg", "tiff", "CairoPNG", "CairoJPEG", "CairoTIFF"),
    raster_quality = 1,
    raster_device_param = list()) {

    if(!is.null(panel.fun)) {
        warning("`panel.fun` is deprecated, please use `panel_fun`.")
        panel_fun = panel.fun
    }
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

    n_arg = length(as.list(args(panel_fun))) - 1

    raster_device = match.arg(raster_device)[1]
    
    for(chr in fa) {
        .GENOMIC_LAYOUT$current_fa = chr
        .GENOMIC_LAYOUT$current_track = track
        
        if(clip) {
        	vp = qq("@{chr}_track_@{track}_datavp_clip_@{i_plot}")
        } else {
        	vp = qq("@{chr}_track_@{track}_datavp_@{i_plot}")
        }

        add_panel_fun = function(gr) {
            seekViewport(name = vp)

            data_xscale = get_cell_meta_data("extended_xlim")
            data_yscale = get_cell_meta_data("extended_ylim")

            if(use_raster) {
                # write the image into a temporary file and read it back
                device_info = switch(raster_device,
                    png = c("grDevices", "png", "readPNG"),
                    jpeg = c("grDevices", "jpeg", "readJPEG"),
                    tiff = c("grDevices", "tiff", "readTIFF"),
                    CairoPNG = c("Cairo", "png", "readPNG"),
                    CairoJPEG = c("Cairo", "jpeg", "readJPEG"),
                    CairoTIFF = c("Cairo", "tiff", "readTIFF")
                )
                if(!requireNamespace(device_info[1])) {
                    stop(paste0("Need ", device_info[1], " package to output image."))
                }
                if(!requireNamespace(device_info[2])) {
                    stop(paste0("Need ", device_info[2], " package to read image."))
                }
                # can we get the size of the heatmap body?
                panel_width = convertWidth(unit(1, "npc"), "bigpts", valueOnly = TRUE)
                panel_height = convertHeight(unit(1, "npc"), "bigpts", valueOnly = TRUE)
                if(panel_width <= 0 || panel_height <= 0) {
                    stop("The width or height of the raster image is zero, maybe you forget to turn off the previous graphic device or it was corrupted. Run `dev.off()` to close it.")
                }
                temp_image = tempfile(pattern = paste0(".gtrellis_panel_", vp), tmpdir = ".", fileext = paste0(".", device_info[2]))
                #getFromNamespace(raster_device, ns = device_info[1])(temp_image, width = heatmap_width*raster_quality, height = heatmap_height*raster_quality)
                device_fun = getFromNamespace(raster_device, ns = device_info[1])
                do.call("device_fun", c(list(filename = temp_image, width = panel_width*raster_quality, height = panel_height*raster_quality), raster_device_param))
                pushViewport(viewport(xscale = data_xscale, yscale = data_yscale))
            }

            if(n_arg == 0){
                panel_fun()
            } else {
                panel_fun(gr)
            }

            if(use_raster) {
                dev.off()
                image = getFromNamespace(device_info[3], ns = device_info[2])(temp_image)
                image = as.raster(image)
                grid.raster(image, width = unit(1, "npc") - unit(2, "bigpts"), height = unit(1, "npc") - unit(2, "bigpts"))
                file.remove(temp_image)
            }
        }
        if(is.null(gr)) {
            add_panel_fun(gr)
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
                    add_panel_fun(sub_gr)
                }
            } else {
                sub_gr = gr[gr[[1]] == chr, , drop = FALSE]
                sub_gr = sub_gr[is_intersected(sub_gr[[2]], sub_gr[[3]], extended_xlim[1], extended_xlim[2]), , drop = FALSE]
                if(nrow(sub_gr)) {
                    add_panel_fun(sub_gr)
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

    i = which(.GENOMIC_LAYOUT$fa %in% category)
    row = which(sapply(.GENOMIC_LAYOUT$p, function(ind) any(ind == i)))
    column = which(.GENOMIC_LAYOUT$p[[row]] == i)
    
    switch(name,
           name = category,
           xlim = .GENOMIC_LAYOUT$xlim[i, ],
           ylim = .GENOMIC_LAYOUT$ylim[track, ],
           extended_xlim = .GENOMIC_LAYOUT$extended_xlim[i, ],
           extended_ylim = .GENOMIC_LAYOUT$extended_ylim[track, ],
           original_xlim = .GENOMIC_LAYOUT$original_xlim[i, ],
           original_ylim = .GENOMIC_LAYOUT$original_ylim[track, ],
           column = column,
           row = row,
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
        add_track(track = i, panel_fun = function(gr) {
            nm = get_cell_meta_data("name")
            grid.text(qq("@{nm}\ntrack:@{i}"), unit(0.5, "npc"), unit(0.5, "npc"))
        })
    }
}


is_intersected = function(start, end, lim_start, lim_end) {
    l = (lim_start >= start & lim_start <= end) |
        (lim_end >= start & lim_end <= end) | 
        (lim_start <= start & lim_end >= end)
    return(l)
}

# == title
# The index of current track
# 
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
current_track = function() {
    get_cell_meta_data("track")
}
