#' Creator of significance labels
#'
#' Creates significance labelings from p values.
#'
#' In data visualization, traditional combination of symbols
#' such as ** or + 
#' are used to present statistical significance.
#' When denoting the p values themselves,
#' there also is a labeling tradition
#' like "p < .05" or "p = .82".
#' [pval2lab()] creates these labelings from raw p values
#' which are then used for data plotting.
#'
#' @param p Numerics. Raw p values.
#'
#' @return Numerics.
#'   Labels for given p values.
#'   Extract and use [base::names()] of the returning vector for
#'   symbol labeling such as ** or +.
#'
#' @examples
#'   p <- c(0.9, 0.0777, 0.04, 0.009, 0.00002)
#'   pval2lab(p)
#'
#' @keywords utilities
#'
#' @export

pval2lab <- function(p) {

lab <- c(
	"NA" = "p = NA",
	"XS" = "p < .001",
	"HS" = "p < .01",
	"S" = "p < .05",
	"TR" = "p < .10",
	"NS" = "n.s.")
val <- paste("p = ", substring(sprintf("%.3f", p), 2), sep="")
sym <- c(
	"NA" = "",
	"XS" = "***",
	"HS" = "**",
	"S" = "*",
	"TR" = "+",
	"NS" = "")

sig <- ifelse(is.na(p), "NA",
	ifelse(p < 0.001, "XS",
	ifelse(p < 0.01, "HS",
	ifelse(p < 0.05, "S",
	ifelse(p < 0.1, "TR", "NS")))))
l <- lab[sig]
l[sig == "TR"] <- val[sig == "TR"]
names(l) <- sym[sig]

return(l)
}

