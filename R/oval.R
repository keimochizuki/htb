oval <- function(

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

polygon(xs, ys, ...)

invisible(list(x = xs, y = ys))
}

