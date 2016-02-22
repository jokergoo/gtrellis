
partition = function(x, n) {

	.partition = function(x, n, r = 1) {
		w = sum(x)/n * r

		i_part = 1
		s = 0
		part = NULL
		for(i in seq_along(x)) {
			if(s + x[i] < w + x[i]/2) {
				part[i] = i_part
				s = s + x[i]
			} else {
				i_part = i_part + 1
				part[i] = i_part
				s = x[i]
			}
		}
		return(part)
	}

	error = function(x, p) {
		# n = max(p)
		# w = sum(x)/n
		m = max(tapply(x, p, sum))
		#mean(abs(tapply(x, p, sum) - w))
		v = abs(tapply(x, p, sum) - m)
		var(v)
	}

	p_list = list()
	error_list = NULL
	r = seq(0.8, 1.2, by = 0.1)
	for(i in seq_along(r)) {
		p_list[[i]] = .partition(x, n, r[i])
		error_list[i] = error(x, p_list[[i]])
	}

	l = sapply(p_list, max) <= n
	if(sum(l) == 0) {
		i = which.min(error_list)[1]
		p = p_list[[i]]
		p[p > n] = n
		return(p)
	}
	p_list = p_list[l]
	error_list = error_list[l]
	i = which.min(error_list)[1]
	return(p_list[[i]])
}

# plot_partition = function(x, p) {
# 	n = max(p)
# 	max = max(tapply(x, p, sum))
# 	plot(NULL, xlim = c(0, 1), ylim = c(0, n), type = "n")
# 	for(i in 1:n) {
# 		xx = x[p == i]
# 		segments(cumsum(c(0, xx[-length(xx)]))/max, n-i+1, cumsum(xx)/max, n-i+1, col = rand_color(length(xx)), lwd = 4)
# 		text((cumsum(xx) - xx/2)/max, n-i+1, names(xx))
# 	}
# }

# library(circlize)
# chr.len = read.cytoband()$chr.len
# for(i in 1:10) {
# 	p = partition(chr.len, i)
# 	plot_partition(chr.len, p)
# }
