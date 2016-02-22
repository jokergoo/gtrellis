INDEX_ENV = new.env()

INDEX_ENV$I_PLOT = 0

get_plot_index = function() {
	paste0("plot", INDEX_ENV$I_PLOT)
}

increase_plot_index = function() {
	INDEX_ENV$I_PLOT = INDEX_ENV$I_PLOT + 1
}
