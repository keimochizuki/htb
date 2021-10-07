htbPlotHis <- function(

	his = NULL,
	ras = NULL,
	index = NULL,
	prop = c(0.8, 0.4, 50),

	xlim = NULL,
	ylim = NULL,

	histy = "l",
	hiscol = NULL,
	hislty = "solid",
	hislwd = 2,
	hispch = 15,
	hisname = NULL,
	hiscex = 0.5,
	hisbty = "o",
	hisleg = "bottomright",

	sdty = "n",
	sdcol = NULL,
	sdlty = "22",
	sdlwd = 1,

	rasty = "p",
	rascol = NULL,
	raspch = ".",
	rassp = 2,
	nlines = NULL,

	evcol = NULL,
	evpch = 15,
	evname = NULL,
	evcex = 0.5,
	evbty = "o",
	evleg = "topright",

	sqlim = NULL,
	sqpanel = c(1, 1),
	sqcol = "gray90",

	vlat = NULL,
	vlcol = "gray30",
	vllty = "solid",

	xlab = "Time [ms]",
	ylab = NULL,

	bty = "l",
	las = 1,
	...

) {

if ((!is.null(his)) && (class(his) %in% c("htbHis", "htbRoc"))) {
	his <- list(his)
}
if ((!is.null(ras)) && (class(ras) == "htbRas")) {
	ras <- list(ras)
}

if (!is.null(index)) {
	if (!is.null(his)) {
		his <- lapply(X = his, FUN = htbExtractCond, index)
	}
	if (!is.null(ras)) {
		ras <- lapply(X = ras, FUN = htbExtractCond, index)
	}
}



npanel <- max(length(his), length(ras))
if (npanel == 0) {
	stop("htbPlotHis: Neither his/ras provided")
}
ncond_his <- sapply(X = his, FUN = function(z) { length(z$da$y) })
ncond_ras <- sapply(X = ras, FUN = function(z) { length(z$da) })
ntrl_ras <- lapply(X = ras, FUN = function(z) {
	sapply(X = z$da, FUN = length) })

if (!is.null(ras)) {
	rassptotal <- (ncond_ras - 1) * rassp
	if (is.null(nlines)) {
		nlines <- mapply(FUN = function(ncond, yspt) {
			table(rep(1:ncond, length.out = prop[3] - yspt))
		}, ncond_ras, rassptotal, SIMPLIFY = FALSE)
	} else if (any(nlines == "all")) {
		nlines <- ntrl_ras
		prop[3] <- max(sapply(X = nlines, FUN = sum) + rassptotal)
	}
	nlines <- rep(nlines, length.out = npanel)
	nlines <- mapply(FUN = function(a, b) {
		pmin(a, b) }, nlines, ntrl_ras, SIMPLIFY = FALSE)
}



if (is.null(hiscol) && !is.null(rascol)) {
	hiscol <- rascol
} else if (is.null(rascol) && !is.null(hiscol)) {
	rascol <- hiscol
}

reppar <- function(tmpp, tmpn) {
	if (!is.list(tmpp)) {
		tmpp <- list(tmpp)
	}
	mapply(FUN = function(x, y) {
		rep(x, length.out = y)
	}, rep(tmpp, length.out = npanel), tmpn, SIMPLIFY = FALSE)
}

if (!is.null(his)) {
	if (is.null(hiscol)) {
		hiscol <- lapply(X = ncond_his, FUN = htb.colors)
	}
	hiscol <- reppar(hiscol, ncond_his)
	hislty <- reppar(hislty, ncond_his)
	hislwd <- reppar(hislwd, ncond_his)

	if (is.null(sdcol)) {
		sdcol <- hiscol
	}
	sdcol <- reppar(sdcol, ncond_his)
	sdlty <- reppar(sdlty, ncond_his)
	sdlwd <- reppar(sdlwd, ncond_his)
}

if (!is.null(ras)) {
	if (is.null(rascol)) {
		rascol <- lapply(X = ncond_ras, FUN = htb.colors)
	}
	rascol <- reppar(rascol, ncond_ras)
}



if (is.null(xlim)) {
	xlim <- list()
	for (i in 1:npanel) {
		hp <- lapply(X = his[[i]]$param, FUN="[[", "xlim")
		rp <- lapply(X = ras[[i]]$param, FUN="[[", "xlim")
		xlim[[i]] <- range(unlist(rp), unlist(hp), na.rm = TRUE)
	}
} else if (!is.list(xlim)) {
	xlim <- list(xlim)
}
xlim <- rep(xlim, length.out = npanel)



if (is.null(ras)) {
	prop[1] <- 1
}
if (is.null(his)) {
	prop[2] <- 1
}
if (!is.null(his)) {
	h <- unlist(mapply(FUN = function(tmphis, tmpxlim) {
		i <- (tmphis$da$x >= tmpxlim[1]) & (tmphis$da$x <= tmpxlim[2])
		y <- sapply(X = tmphis$da$y, FUN="[", i)
		if ((sdty != "n") && any(names(tmphis$da) == "s")) {
			s <- sapply(X = tmphis$da$s, FUN="[", i)
			y <- c(y - s, y + s)
		}
		return(y)
	}, his, xlim))
}
if (is.null(ylim)) {
	if (!is.null(his)) {
		ylim <- c(0, max(h, na.rm = TRUE))
	} else if (prop[2] == 1) {
		ylim <- c(0, max(sapply(X = nlines, FUN = sum) + rassptotal))
	} else {
		ylim <- c(0, 1)
	}
} else if (ylim[1] == "auto") {
	ylim <- range(h, na.rm = TRUE)
}
ylim[2] <- ylim[1] + diff(ylim) / prop[1]
ysep <- diff(ylim) * prop[2] / prop[3]



if (is.null(evname)) {
	evname <- lapply(X = ras, FUN = function(z) { names(z$ev) })
	evname <- unique(unlist(evname))
}

if (is.null(ylab) && !is.null(his)) {
	ylab <- his[[1]]$param[[1]]$type
}



if (rasty=="p") {
	raster <- function(x, y, pch = raspch, ...) {
		graphics::points(x = x, y = y, pch = pch, ...) }
} else if (rasty=="l") {
	#raster <- function(x, y, ...) {
	raster <- function(x, y, pch = raspch, ...) {
		graphics::segments(x0 = x, y0 = y, x1 = x, y1 = y-ysep, lend = "butt", ...) }
}


browser()
xshift <- htbPlotWindow(xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab,
	sqlim = sqlim, sqpanel = sqpanel, sqcol = sqcol,
	vlat = NULL, # Leave vertical lines later
	bty = bty, las = las, ...)
usr <- graphics::par("usr")



n_ev <- 0
xs_ev <- list()
ys_ev <- list()
uni_ev <- NULL
for (i in 1:npanel) {
	tmpras <- ras[[i]]
	tmphis <- his[[i]]

	da <- tmpras$da
	ev <- tmpras$ev
	param <- tmpras$param

	if (!is.null(tmpras)) {
		tmpy <- usr[4]
		for (j in seq(along = da)) {
			if (nlines[[i]][j] != 0) {
				xs <- unlist(da[[j]][1:nlines[[i]][j]])
				ys <- rep(seq(tmpy, tmpy - (nlines[[i]][j] - 1) * ysep, by = -ysep),
					sapply(X = da[[j]][1:nlines[[i]][j]], FUN = length))
				k <- (xs >= xlim[[i]][1]) & (xs <= xlim[[i]][2])
				xs <- xs[k] + xshift[i]
				ys <- ys[k]
				raster(x = xs, y = ys, col = rascol[[i]][j])
			}
			tmpy <- tmpy - (nlines[[i]][j] + rassp) * ysep
		}

		if ((!is.null(ev)) && (sum(nlines[[i]]) != 0)) {
			for (j in evname) {
				if (!is.null(ev[[j]])) {
					xs <- unlist(mapply(FUN = function(x, n) {
						x[1:n] }, ev[[j]], nlines[[i]]))
					ys <- seq(usr[4], usr[4] - (sum(nlines[[i]]) - 1) * ysep, by = -ysep) -
						rep(seq(0, along = da), nlines[[i]]) * rassp * ysep
					k <- (xs >= xlim[[i]][1]) & (xs <= xlim[[i]][2])
					if (sum(k, na.rm = TRUE) == 0) {
						next
					}
					n_ev <- n_ev + 1
					xs_ev[[n_ev]] <- xs[k] + xshift[i]
					ys_ev[[n_ev]] <- ys[k]
					uni_ev[n_ev] <- j
				}
			}
		}
	}

	if (!is.null(tmphis)) {
		xs <- tmphis$da$x
		k <- (xs >= xlim[[i]][1]) & (xs <= xlim[[i]][2])
		xs <- xs[k] + xshift[i]
		for (j in rev(seq(along = tmphis$da$y))) {
			ys <- tmphis$da$y[[j]][k]

			if (sdty %in% c("l", "b", "p")) {
				ss <- tmphis$da$s[[j]][k]
				ypm <- list(ys + ss, pmax(ys - ss, 0))
				if (!is.null(ss)) {
					if (sdty == "l") {
						lapply(X = ypm, FUN=function(y) {
							graphics::lines(xs, y, col = sdcol[[i]][j], lty = sdlty[[i]][j], lwd = sdlwd[[i]][j]) })
					} else if (sdty == "b") {
						graphics::segments(xs, ypm[[1]], y1 = ypm[[2]],
							col = sdcol[[i]][j], lty = sdlty[[i]][j], lwd = sdlwd[[i]][j])
					} else if (sdty == "p") {
						graphics::polygon(c(xs, rev(xs)), c(ypm[[1]], rev(ypm[[2]])),
							border = NA, col = sdcol[[i]][j])
					}
				}
			}
			if (histy == "h") {
				b <- tmphis$param[[j]]$bin
				graphics::rect(xs - b/2, 0, xs + b/2, ys, col = hiscol[[i]][j], border = hiscol[[i]][j])
			} else {
				graphics::lines(xs, ys, col = hiscol[[i]][j], lty = hislty[[i]][j], lwd = hislwd[[i]][j])
			}
		}
	}
}



if (n_ev > 0) {
	if (is.null(evcol)) {
		evcol <- grDevices::rainbow(length(uni_ev))
	} else {
		evcol <- rep(evcol, length.out = n_ev)
	}
	raster(unlist(xs_ev), unlist(ys_ev), cex = evcex, pch = evpch,
		col = rep(evcol, sapply(X = xs_ev, FUN = length)))

	if (!is.null(evleg)) {
		graphics::legend(x = evleg, legend = uni_ev, col = evcol,
			pch = evpch, cex = evcex, bty = evbty, bg = "white", inset = 0.02)
	}
}
if (!is.null(hisleg)) {
	if (is.null(hisname) && (npanel == 1)) {
		hisname <- sapply(X = his[[1]]$param, FUN = "[[", "title")
	}
	if (length(hisname) > 0) {
		graphics::legend(x = hisleg, legend = hisname, col = hiscol[[1]],
			pch = hispch, cex = hiscex, bty = hisbty, bg = "white", inset = 0.02)
	}
}

if (!is.null(vlat)) {
	if (!is.list(vlat)) {
		vlat <- list(vlat)
	}
	vlpanel <- rep(seq(along = vlat), sapply(X = vlat, FUN = length))
	vlat <- unlist(vlat)
	nvl <- length(vlat)
	vlcol <- rep(vlcol, length.out = nvl)
	vllty <- rep(vllty, length.out = nvl)
	graphics::segments(vlat + xshift[vlpanel], usr[3], y1 = usr[4],
		col = vlcol, lty = vllty)
}

invisible(xshift)
}



htbGetPop <- function(

	obj,
	type = c("sd", "se"),
	...

) {

obj <- obj[sapply(X = obj, FUN = length) != 0]
pop <- obj[[1]]
type <- type[1]

if (class(pop) == "htbHis") {
	tmpx <- lapply(X = obj, FUN = function(tmphis) { tmphis$da$x })
	if (!all(diff(sapply(X = tmpx, FUN = length)) == 0)) {
		stop("htbGetPop: Numbers of time points differ between elements")
	}
	tmpx <- do.call(cbind, tmpx)
	if (!all(apply(X = tmpx, MARGIN = 1, FUN = diff) == 0)) {
		stop("htbGetPop: Time points differ between elements")
	}
	pop$da$s <- list()

	for (i in seq(along = pop$da$y)) {
		tmpy <- lapply(X = obj, FUN = function(tmphis) { tmphis$da$y[[i]] })
		if (!all(diff(sapply(X = tmpy, FUN=length)) == 0)) {
			stop("htbGetPop: Numbers of sample differ between elements")
		}
		tmpy <- do.call(cbind, tmpy)
		tmpy <- tmpy[, !is.na(tmpy[1,]), drop = FALSE]
		if (ncol(tmpy) == 0) {
			pop$da$y[[i]] <- rep(NA, length(pop$da$x))
			pop$da$s[[i]] <- rep(NA, length(pop$da$x))
			next
		} else {
			pop$da$y[[i]] <- rowMeans(tmpy, na.rm = TRUE)
			pop$da$s[[i]] <- apply(X = tmpy, MARGIN = 1, FUN = stats::sd, na.rm = TRUE) /
				switch(type, sd = 1, se = sqrt(ncol(tmpy)))
		}
	}
	names(pop$da$s) <- names(pop$da$y)
}

return(pop)
}

