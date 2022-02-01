htbCountSpike <- function(

	ras,
	from = NULL,
	to = NULL,
	type = "m"

) {

n <- sapply(X = ras$da, FUN = length)
for (i in c("from", "to")) {
	if (is.null(get(i))) {
		assign(i, mapply(FUN = function(z1, z2) {
			rep(z1$xlim[ifelse(i == "from", 1, 2)], z2)
		}, ras$param, n, SIMPLIFY = FALSE))
	} else if (is.character(get(i))) {
		assign(i, ras$ev[[get(i)]])
		if (is.null(get(i))) {
			stop("Non-existing event assigned as an interval")
		}
	} else {
		assign(i, lapply(X = n, FUN = rep, x = get(i)))
	}
}
l <- mapply(FUN = function(z1, z2) { z2 - z1 }, from, to, SIMPLIFY = FALSE)

s <- mapply(FUN = function(ss, fs, ts) {
	if (length(ss) == 0) {
		return(NULL)
	} else {
		return(mapply(FUN = function(x, y, z) {
			sum((x >= y) & (x < z)) }, ss, fs, ts))
	}
}, ras$da, from, to, SIMPLIFY = FALSE)

if (type == "m") {
	s <- mapply(FUN = function(x, y) { x / y * 1000 }, s, l, SIMPLIFY = FALSE)
}

return(s)
}

