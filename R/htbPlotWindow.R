htbPlotWindow <- function(

	xlim,
	ylim,
	xsp = NULL,
	xtick = NULL,

	sqlim = NULL,
	sqpanel = c(1, 1),
	sqcol = "gray90",

	vlat = NULL,
	vlcol = "gray30",
	vllty = "solid",

	xaxt = "s",
	xaxs = "i",
	xlab = "",
	ylab = "",
	bty = "l",
	slash = TRUE,
	...

) {

if (!is.list(xlim)) {
	if (length(xlim) == 2) {
		xlim <- list(xlim)
	} else {
		stop("htbPlotWindow: Inappropriate designation of <xlim>")
	}
}
l <- sapply(X = xlim, FUN = diff)
npanel <- length(xlim)

if (is.null(xtick)) {
	xtick <- lapply(X = xlim, FUN = axisTicks, log = FALSE)
} else if (!is.list(xtick)) {
	xtick <- list(xtick)
}

if (is.null(xsp)) {
	xsp <- sum(l) * 0.04
}
xlim_all <- c(0, sum(l, xsp * (npanel - 1)))

xshift <- cumsum(l)
xshift <- c(0, xshift[-npanel]) +
	xsp * seq(0, length.out = npanel) -
	sapply(X = xlim, FUN="[", 1)

plot(0, 0, type = "n", xlim = xlim_all, ylim = ylim,
	xlab = xlab, ylab = ylab,
	bty = "n", xaxt = "n", xaxs = xaxs, yaxt = "n", ...)

if (!is.null(sqlim)) {
	sqlim <- matrix(unlist(sqlim), ncol = 2, byrow = TRUE)
	sqpanel <- matrix(unlist(sqpanel), ncol = 2, byrow = TRUE)
	nsq <- nrow(sqlim)
	sqcol <- rep(sqcol, length.out = nsq)
	rect(xleft = sqlim[, 1] + xshift[sqpanel[, 1]], ybottom = par("usr")[3],
		xright = sqlim[, 2] + xshift[sqpanel[, 2]], ytop = par("usr")[4],
		col = sqcol, border = NA)
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
	segments(vlat + xshift[vlpanel], par("usr")[3], y1 = par("usr")[4],
		col = vlcol, lty = vllty)
}

if (xaxt != "n") {
	at <- unlist(xtick)
	axis(1, at = at + rep(xshift, sapply(X = xtick, FUN = length)),
		labels = at, xaxt = xaxt, ...)
}
axis(2, ...)
box(bty = bty, ...)

if (slash && (npanel > 1)) {
	sapply(X = xshift[-1] + sapply(X = xlim, FUN = "[", 1)[-1] - xsp / 2,
		FUN = druSlashAxis, w = 0.01, h = 0.03, side = 1)
}

return(xshift)
}

