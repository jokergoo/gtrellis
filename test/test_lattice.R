library(circlize)

bed = generateRandomBed(nr = 1000)

initialize_layout()
initialize_layout(chromosome = c("chr1", "chr3"))
initialize_layout(track_number = 3)
initialize_layout(track_number = 3, track_height = c(1, 2, 3))
initialize_layout(track_number = 3, track_axis = c(TRUE, FALSE, FALSE))
initialize_layout(track_number = 3, track_ylim = c(0, 3, -4, 4, 0, 1000000))

initialize_layout(track_number = 3, main = "xxxxxx", track_ylab = c("aaaaa", "bbbbbb", "ccccccc"))


initialize_layout(track_number = 3, ncol = 3, 
	track_axis = c(FALSE, TRUE, FALSE), track_height = c(2, 10, 2), track_ylim = c(0, 1, 0, 8, 0, 1))
add_graphics_on_grid(i_track = 1, panel.fun = function(gr){
	chr = get_current_grid_meta_data("chr")
	grid.rect(gp = gpar(fill = "#EEEEEE"))
	grid.text(chr)
})
add_graphics_on_grid(bed, i_track = 2, panel.fun = function(gr) {
	df = rainfallTransform(gr[2:3])
	x = (df[[1]] + df[[2]])/2
	y = log10(df[[3]])
	grid.points(x, y, pch = 16)
})
cytoband_df = read.cytoband()$df
add_graphics_on_grid(cytoband_df, i_track = 3, panel.fun = function(gr) {
	cytoband_chr = gr
	grid.rect( cytoband_chr[[2]], unit(0, "npc"),
	            width = cytoband_chr[[3]] - cytoband_chr[[2]], height = unit(1, "npc"),
	            default.units = "native", hjust = 0, vjust = 0,
				gp = gpar(fill = cytoband.col(cytoband_chr[[5]])) )
	grid.rect(min(cytoband_chr[[2]]), unit(0, "npc"),
		      width = max(cytoband_chr[[3]]) - min(cytoband_chr[[2]]), height = unit(1, "npc"),
		      default.units = "native", hjust = 0, vjust = 0)
})

