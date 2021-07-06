axslash <- function(

	at,
	tick = 0.5,
	sep = 0.5,
	deg = 90,

	fg = "black",
	bg = "white",

	side = 1,
	...

) {

if (wrapdeg(deg) == 0) {
	stop("axslash: <deg> must be non-zero degree")
}

n <- length(at)
cxy <- par("cxy")
usr <- par("usr")
pin <- par("pin")
asp <- (usr[4] - usr[3]) / (usr[2] - usr[1]) * pin[1] / pin[2]

h <- cxy[2] * tick * 0.5 # proportion of line-text height
w <- h / tan(deg / 180 * pi) / asp
s <- cxy[1] * sep * 0.5 # proportion of line-text width

if (side %in% c(1, 3)) {
	# horizontal axis
	x <- w * c(-1, 1, 1, -1) + s * c(-1, -1, 1, 1)
	y <- h * c(-1, 1, 1, -1)

	at <- cbind(x = at, y = rep(ifelse(side == 1, usr[3], usr[4]), n))
} else {
	# vertical axis
	h <- h / asp
	w <- w * asp
	s <- s * asp
	x <- h * c(-1, 1, 1, -1)
	y <- w * c(-1, 1, 1, -1) + s * c(-1, -1, 1, 1)

	at <- cbind(x = rep(ifelse(side == 2, usr[1], usr[2]), n), y = at)
}

for (i in 1:n) {
	polygon(at[i, "x"] + x, at[i, "y"] + y, border = NA, col = bg, xpd = NA)
	lines(at[i, "x"] + x[1:2], at[i, "y"] + y[1:2], col = fg, xpd = NA, ...)
	lines(at[i, "x"] + x[3:4], at[i, "y"] + y[3:4], col = fg, xpd = NA, ...)
}

invisible()
}

