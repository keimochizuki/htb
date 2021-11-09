#' Adjuster of angular values
#'
#' Wraps and adjusts angular values within a given range.
#'
#' Angular values (degrees and radians) are one of the popular parameters
#' you need to handle during analyses,
#' especially in visual tasks and 2-D motor tasks.
#' [wrapdeg()] wraps the given angular values into a given range of degree
#' like [-180, 180) or [0, 360).
#' It allows left-open ranging, e.g., (-180, 180]
#' when `leftopen` is `TRUE`.
#' It also allows radian operation when `rad` is `TRUE`.
#'
#' @param deg Numerics. Angular values you want to adjust.
#' @param min A numeric. The minimal value (left limit) of angular range.
#' @param leftopen A logical. Whether to use left-open ranging.
#'   By default `FALSE`, meaning that [min, min + 360) or
#'   [min, min + 2 * pi) is used as accepted range.
#'   When set `TRUE`, (min, min + 360] or (min, min + 2 * pi]
#'   is used instead.
#' @param rad A logical. Whether the values are in radian unit or not.
#'
#' @return Numerics.
#'   Angular values adjusted to fit within the given range.
#'
#' @examples
#'   d <- c(-120, -45, 0, 90, 180, 360)
#'   r <- d / pi * 180
#'   wrapdeg(d)
#'   wrapdeg(d, -180)
#'   wrapdeg(d, -180, leftopen = TRUE)
#'   wrapdeg(r, -pi, rad = TRUE)
#'
#' @keywords math
#'
#' @export

wrapdeg <- function(

	deg,
	min = 0,
	leftopen = FALSE, # default [min, max)
	rad = FALSE

) {

unit <- ifelse(rad, 2 * pi, 360)
deg <- deg - min
deg <- deg - (deg %/% unit) * unit
if (leftopen) {
	deg[deg == 0] <- unit
}
deg <- deg + min

return(deg)
}

