htbGetHis <- function(

	ras,
	xlim = NULL,
	bin = 200,
	sep = 10,
	type = c("rectangular", "histogram"),
	...

) {

type <- type[1]

if ((class(ras) != "htbRas") && all(sapply(X = ras, FUN = class) == "htbRas")) {
	his <- list()
	for (i in seq(along=ras)) {
		his[[i]] <- htbGetHis(ras[[i]], xlim = xlim,
			bin = bin, sep = sep, type = type, ...)
	}
	names(his) <- names(ras)
	return(his)
}

da <- ras$da
param <- ras$param
n <- length(da)

if (is.null(bin)) {
	bin <- sep
} else if (is.null(sep)) {
	sep <- bin
}
if (type == "histogram") {
	sep <- bin
}

if (is.null(xlim)) {
	xlims <- sapply(X = param, FUN="[[", "xlim")
	xlim <- c(min(xlims[1,]), max(xlims[2,]))
}



y <- list()
s <- list()
if (attributes(ras)$type == "spike") {
	x <- seq(xlim[1], xlim[2], by = sep)
	for (i in seq(along = da)) {

		start <- pmax(x - bin / 2, xlim[1])
		end <- pmin(x + bin / 2, xlim[2])
		t_spike <- unlist(da[[i]])
		n_spike <- length(t_spike)
		n_trial <- length(da[[i]])
		if (n_trial != 0) {
			if (any(type == c("rectangular", "histogram"))) {
				y[[i]] <- mapply(FUN = function(s, e) {
					sum((t_spike >= s) & (t_spike < e))
				}, start, end) / n_trial / (end - start) * 1000
			}
		} else {
			y[[i]] <- rep(NA, length(x))
		}
		pars[[i]]$xlim <- xlim
		pars[[i]]$bin <- bin
		pars[[i]]$sep <- sep
		pars[[i]]$type <- "Firing Rate"
	}
	names(y) <- names(da)
	his <- list(da = list(x = x, y = y), param = param)

} else if (attributes(ras)$type == "analog") {
	x <- seq(xlim[1], xlim[2], by = 1)
	for (i in seq(along = da)) {
		if (length(da[[i]]) != 0) {
			tmp <- do.call(cbind, da[[i]])
			y[[i]] <- rowMeans(tmp)
			s[[i]] <- apply(X = tmp, MARGIN = 1, FUN = sd)
		} else {
			y[[i]] <- rep(NA, length(x))
			s[[i]] <- rep(NA, length(x))
		}
		pars[[i]]$xlim <- xlim
		pars[[i]]$bin <- 1
		pars[[i]]$sep <- 1
		pars[[i]]$type <- "Analog Value"
	}
	names(y) <- names(da)
	names(s) <- names(da)
	his <- list(da = list(x = x, y = y, s = s), param = param)

}
class(his) <- "htbHis"

return(his)
}

