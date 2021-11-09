#' Drawer of ellipse
#'
#' Draws an ellipse with independent major and minor radii.
#'
#' Drawing of circles can be done by the combination of
#' coordinate calculation and [graphics::polygon()] function.
#' [ellipse()] further provides an easy way to draw an ellipse (oval circle)
#' with independent two radii along its major and minor axes.
#'
#' @param a A numeric. Radius along the major axis.
#' @param b A numeric. Radius along the minor axis.
#' @param x A numeric. The horizontal center of the ellipse.
#' @param y A numeric. The vertical center of the ellipse.
#' @param theta A numeric. The obliqueness of the ellipse in degree.
#' @param sep A numeric. The interval of edge drawing in degree.
#'   By default `1` meaning the ellipse is actually drawn as
#'   a 360-gon (triacosiahexecontagon)
#'   because of 1-degree separation between each point.
#' @param ... Other arguments passed to [graphics::polygon()] function.
#'
#' @return A list.
#'   The first and second element respectively corresponds to
#'   x- and y-coodinates of the ellipse's edge.
#'
#' @examples
#'   plot(0, 0, type = "n", bty = "l",
#'     xlab = "x", ylab = "y",
#'     xlim = c(0, 100), ylim = c(0, 100))
#'   ellipse(25, 10, 30, 20)
#'   text(30, 20, "ellipse(25, 10, 30, 20)")
#'   ellipse(35, 20, 60, 70, 30,
#'     lwd = 3, col = "gray")
#'   text(60, 70, "ellipse(35, 20, 60, 70, 30)")
#'
#' @keywords aplot
#'
#' @export

ellipse <- function(

	a,
	b = a,
	x = 0,
	y = 0,
	theta = 0,
	sep = 1,
	...

) {

d <- seq(from = 0, to = 360, by = sep) / 180 * pi
xs <- a * cos(d)
ys <- b * sin(d)

if (theta != 0) {
	theta <- theta / 180 * pi
	xs2 <- xs * cos(theta) - ys * sin(theta)
	ys <- xs * sin(theta) + ys * cos(theta)
	xs <- xs2
}
xs <- xs + x
ys <- ys + y

graphics::polygon(xs, ys, ...)

invisible(list(x = xs, y = ys))
}

