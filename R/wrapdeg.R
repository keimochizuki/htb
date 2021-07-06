wrapdeg <- function(

	deg,
	left = 0,
	leftopen = FALSE, # default [min, max)
	rad = FALSE

) {

unit <- ifelse(rad, 2 * pi, 360)
deg <- deg - (deg %/% unit) * unit
if (leftopen) {
	deg[deg == 0] <- unit
}
deg <- deg + left

return(deg)
}

