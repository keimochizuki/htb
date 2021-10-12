#' Encapsulated PostScript device
#'
#' Graphics device for an EPS (Encapsulated PostScript) format.
#'
#' [eps()] is a wrapper for built-in [grDevices::postscript()] function in R.
#' It opens a graphics device as an Encapsulated PostScript,
#' with .eps extension and several pre-determined parameters.
#' It is noteworthy that specification of
#' paper and horizontal arguments (as `special` and FALSE)
#' is currently necessary for R to produce
#' proper postscript output.
#' Designating different values for these parameters may
#' cause coordinate problems in using the resultant eps file
#' in other applications such as LaTeX and Adobe Illustrator.
#' (But this might be a candidate for the future bug fixing in R.)
#'
#' @param filename A string. The name of the created eps file.
#'   The extension (.eps) can be omitted.
#' @param width A numeric. The width of the device in inches.
#' @param height A numeric. The height of the device in inches.
#' @param paper A string. The type of the paper designated
#'   in calling postscript function.
#'   Should be `special` (default) to avoid
#'   coordinate disarrangement in using eps file
#'   (e.g., vertical flipping when included in LaTeX).
#' @param horizontal A logical. Whether to rotate
#'   the graphical coordinate.
#'   Should be always FALSE regardless of whether the figure
#'   is horizontally long or vertically tall.
#'   Otherwise, the resulting graphics can be inappropriately
#'   rotated in certain viewers.
#' @param ... Other arguments passed to postscript function.
#'
#' @return NULL
#'
#' @examples
#'   \dontrun{
#'   eps("test", width = 6, height = 4)
#'   example(htb.colors)
#'   dev.off()
#'   }
#'
#' @keywords device
#'
#' @export

eps <- function(

	filename,
	width = 7,
	height = 7,
	paper = "special",
	horizontal = FALSE,
	...

) {

if (!grepl("\\.eps$", filename, ignore.case = TRUE)) {
	filename <- paste(filename, ".eps", sep = "")
}
dir.check(filename)
grDevices::postscript(file = filename, width = width, height = height,
	paper = paper, horizontal = horizontal, ...)

invisible()
}

