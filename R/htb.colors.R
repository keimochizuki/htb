#' Visually friendly color palette
#'
#' Create a vector of n contiguous colors.
#'
#' When visualizing data in a graphical plot,
#' R's built-in [grDevices::rainbow()] function is
#' one of the best and handiest tools to create colors to use.
#' However, when creating a small number of (say, 4 or 5)
#' colors for categorical data groups,
#' the result sometimes contains indistinct colors
#' such as highly bright cyan or yellow.
#' [htb.colors()] deals with this problem,
#' by using a predetermined set of rainbow-like,
#' visually friendly colors.
#' Those colors are carefully chosen with a wish that
#' they be better distinguished
#' even in case of achromatopsia.
#' This may be especially useful when the number
#' of colors are relatively few (e.g., < 10).
#' When the number is large,
#' the results of [grDevices::rainbow()] and [htb.colors()]
#' will not differ so much,
#' though the latter will be still a bit quiet (unvivid)
#' compared with the former.
#'
#' In case of conditions you want non-rainbow colors,
#' [htb.colors()] can also produce different set of color types,
#' which you might have happened to see elsewhere.
#' Those colors are not cared for discriminability,
#' so please be cautious.
#'
#' @param n An integer. The number of colors (>= 1)
#'   to be in the palette.
#' @param type A string. The type of the color set.
#'   Default `rainbow` produces rainbow-like colors.
#'   Other available types are `bruna` and `imas`.
#'
#' @return Strings. A vector of color codes in `#xxxxxx` format.
#'
#' @examples
#'   piecol <- function(x) {
#'     pie(rep(1, length(x)), labels = rep("", length(x)),
#'       col = x, border = NA, radius = 0.95)
#'   }
#'
#'   par(mfrow = c(4, 5), mar = rep(0.1, 4))
#'   for (f in c(grDevices::rainbow, htb.colors)) {
#'     sapply(X = c(1:9, 30), FUN = function(n) {
#'       piecol(f(n))
#'       text(0, 0, n, col = "white", font = 2)
#'     })
#'   }
#'
#'   n <- c(10, 6, 12)
#'   types <- c("rainbow", "bruna", "imas")
#'   par(mfrow = c(2, 2))
#'   mapply(FUN = function(z1, z2) {
#'     piecol(htb.colors(z1, type = z2))
#'     text(0, 0, z2, col = "white", font = 2)
#'   }, n, types)
#'
#' @keywords color
#'
#' @export

htb.colors <- function(

	n,
	type = "rainbow"

) {

if (n <= 0) {
	return(character(0))
}

pal <- TRUE
if (type == "rainbow") {
	col <- switch(as.character(n),
		"1" = "#0072B2",
		"2" = c("#E69F00", "#0072B2"),
		"3" = c("#E69F00", "#56B4E9", "#0072B2"),
		"4" = c("#D55E00", "#E69F00", "#56B4E9", "#0072B2"),
		"5" = c("#D55E00", "#E69F00", "#D9CB00", "#56B4E9", "#0072B2"),
		"6" = c("#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2"),
		"7" = c("#E66080", "#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2"),
		"8" = c("#912CEE", "#E66080", "#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2"),
		"9" = c("#912CEE", "#E66080", "#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2", "#30308B"),
		c("#912CEE", "#E66080", "#e03040", "#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2", "#30308B"))
	pal <- n > 10
} else if (type == "bruna") {
	col <- c("#D95D2A", "#F8DF4A", "#1D508E", "#3C782E", "#845C40", "#726F62")
	if (n <= 6) {
		col <- col[1:n]
		pal <- FALSE
	}
} else if (type == "imas") {
	col <- c("#F70F1F", "#0775C4", "#AECECB", "#F29047",
		"#00A752", "#7E51A6", "#FA98BF", "#464B4F",
		"#FCD424", "#A1CA62", "#00B1BB", "#B51D66")
	if (n <= 12) {
		col <- col[1:n]
		pal <- FALSE
	}
} else if (type == "excel") {
	col <- c("#000080", "#FF00FF", "#FFFF00", "#00FFFF",
		"#800080", "#800000", "#008080", "#0000FF")
	if (n <= 8) {
		col <- col[1:n]
		pal <- FALSE
	}
} else if (type == "fuckingexcel") {
	col <- c("#9999FF", "#993366", "#FFFFCC", "#CCFFFF",
		"#660066", "#FF8080", "#0066CC", "#CCCCFF")
	if (n <= 8) {
		col <- col[1:n]
		pal <- FALSE
	}
} else {
	stop("Undefined color `type` designated")
}

if (pal) {
	col <- grDevices::colorRampPalette(col)(n)
}

return(col)
}

