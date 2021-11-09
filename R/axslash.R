#' Drawer of axis double slash
#'
#' Draws a double slash to the axis to indicate a gap in graph range.
#'
#' When creating a graph, there are times that you
#' want to put a gap in the graph's axis
#' so that you can show numerically distant data points
#' in neighbor (of course for the visualization purpose).
#' This function provides an easy way for it,
#' with arbitrary height, spacing and angles of the two slashes.
#'
#' @param at Numerics. The positions to put slashes in the graph.
#'   The values are in the same metrics (scale) to the designated axis.
#'   The points become the center of each double slash.
#' @param tick A numeric. The height of slashes as a fraction
#'   of the default character height.
#'   Note that the default height of the ticks in plotting
#'   is a half (0.5) of the character height.
#' @param sep A numeric. The width between slashes as a fraction
#'   of the default character width.
#' @param deg A numeric. The angle of the slashes
#'   in relation to the direction of the axis.
#'   This must be a non-zero value since 0 degree to the axis direction
#'   results in a complete parallel lines to (and on) the axis.
#'   Setting 90 results in a equal-like mark perpendicular to the axis
#'   instead of slashes.
#' @param fg A string. The color of the slash lines.
#' @param bg A string. The color of the space intervened between the slashes.
#'   This color overwrite the existing axis.
#'   Therefore, the same color to the graph's background
#'   should be naturally used.
#' @param side An integer. The side of the axis to put slashes
#'   in the same way to other R's default plotting functionality
#'   (1:bottom, 2:left, 3:top, 4:right).
#' @param ... Other arguments passed to [graphics::lines()] function.
#'
#' @examples
#'   x <- seq(-3, 3)
#'   z <- 10
#'   y <- pnorm(x)
#'   plot(x, y, type = "o", bty = "l",
#'     xlim = c(min(x), max(x) + 1), xaxt = "n")
#'   points(max(x) + 1, pnorm(z))
#'   axis(1, at = c(x, max(x) + 1), labels = c(x, z))
#'   axslash(max(x) + 0.5, deg = 60)
#'
#' @keywords aplot
#'
#' @export

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
	stop("`deg` must be non-zero degree")
}

n <- length(at)
cxy <- graphics::par("cxy")
usr <- graphics::par("usr")
pin <- graphics::par("pin")
asp <- (usr[4] - usr[3]) / (usr[2] - usr[1]) * pin[1] / pin[2]

h <- cxy[2] * tick * 0.5 # fraction of line-text height
w <- h / tan(deg / 180 * pi) / asp
s <- cxy[1] * sep * 0.5 # fraction of line-text width

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
	graphics::polygon(at[i, "x"] + x, at[i, "y"] + y, border = NA, col = bg, xpd = NA)
	graphics::lines(at[i, "x"] + x[1:2], at[i, "y"] + y[1:2], col = fg, xpd = NA, ...)
	graphics::lines(at[i, "x"] + x[3:4], at[i, "y"] + y[3:4], col = fg, xpd = NA, ...)
}

invisible()
}

