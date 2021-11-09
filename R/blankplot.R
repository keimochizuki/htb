#' Drawer of blank plot area
#'
#' Draws a blank plotting area for further handmade drawings.
#'
#' R's built-in plotting functions such as
#' [graphics::plot()], [graphics::boxplot()] and [graphics::hist()] are
#' so useful that your daily analysis and data visualization can be
#' often completed by only these function.
#' However, there are also times that you want to create
#' more complicated graphs.
#' In such cases, you will first construct a blank plotting are
#' by [graphics::plot()], and then further add required drawings.
#' [blankplot()] can be used as a quick wrapper for this routine procedure.
#' Just use it to create a tabula-rasa plotting region
#' with designated x and y limits,
#' from where your data visualization can start over.
#'
#' @param xlim A pair of numerics. The x limits of the plot.
#' @param ylim A pair of numerics. The y limits of the plot.
#' @param bty A character. The type of box of the plot
#'   passed to [graphics::par()].
#' @param mgp A set of three numerics. The amounts of margin lines
#'   passed to [graphics::par()].
#' @param main A string. The main title (by default a blank).
#' @param sub A string. The sub title (by default a blank).
#' @param xlab A string. The label for the x axis (by default a blank).
#' @param ylab A string. The label for the y axis (by default a blank).
#' @param ... Other arguments passed to [graphics::plot()] function.
#'
#' @examples
#'   n <- 100
#'   r <- seq(0, 2 * pi, length.out = n)
#'   x <- 16 * sin(r)^3
#'   y <- 13 * cos(r) - 5 * cos(2 * r) -
#'     2 * cos(3 * r) - cos(4 * r)
#'   blankplot(range(x), range(y))
#'   col <- rainbow(n)
#'   segments(x[-n], y[-n], x[1], y[1], col = col)
#'   segments(x[-n], y[-n], x[-1], y[-1], col = col, lwd = 2)
#'
#' @keywords hplot
#'
#' @export

blankplot <- function(

	xlim = c(0, 1),
	ylim = c(0, 1),
	bty = "l",
	mgp = c(2.5, 1, 0),
	main = "",
	sub = "",
	xlab = "",
	ylab = "",
	...

) {

graphics::par(bty = bty, mgp = mgp)
graphics::plot(0, 0, type = "n", xlim = xlim, ylim = ylim,
	main = main, sub = sub, xlab = xlab, ylab = ylab, ...)

invisible()
}

