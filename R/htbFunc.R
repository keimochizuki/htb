#' Bulk loader of .R files
#'
#' Sources all the .R files contained in the designamted path(s).
#'
#' Usually your analyses have numbers of functions
#' written in many discrete .R files,
#' possibly contained in different directories.
#' [htbFunc()] sources all of these .R files existing in
#' the designated path(s) all at once using [source()].
#' This will help your coding since you do not need to
#' [source()] each .R files one by one.
#' You can perform any changes in your codes
#' (maybe within multiple .R files) as needed.
#' When the modifications are done,
#' just call [htbFunc()] and all the changes are applied
#' by [source()]ing .R files.
#'
#' Be careful, however, since .R files are actually executed
#' in the standard way by [source()],
#' all of the analysis contents can be immediately started to perform
#' if they are written as scripts.
#' To avoid this, code your analyses as functions in your .R files.
#'
#' @param path Strings. The directories where .R files are put.
#' @param recursive A logical. Whether to recursively search for .R files.
#'
#' @return Strings.
#'   Names of the functions sourced.
#'
#' @examples
#' htbFunc()
#'
#' @keywords utilities
#'
#' @export

htbFunc <- function(

	path = "~/htb/func",
	recursive = TRUE

) {

path <- path.expand(path)
path <- sub("/+$", "", path)

func <- NULL
for (tmp in path) {
	tmpfunc <- list.files(path = tmp,
		pattern = "\\.[Rr]$", full.names = TRUE, recursive = recursive)
	if (length(tmpfunc) == 0) {
		cat("htbFunc: There is no .R script in ", tmp, "\n", sep = "")
	} else {
		invisible(sapply(FUN = source, X = tmpfunc))
		cat("htbFunc: Sourced .R scripts from ", tmp, "\n", sep = "")
		cat(paste("  ", tmpfunc, sep = ""), sep = "\n")
	}
	func <- c(func, tmpfunc)
}

invisible(func)
}

