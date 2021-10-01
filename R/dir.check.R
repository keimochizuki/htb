#' Checker for existing directories for file output
#'
#' Checks for the existance of the directories to where the designated file(s) will be put.
#'
#' While doing your analyses, you may often want several directories
#' to where the output files can be saved.
#' But before saving, you need to first check
#' whether appropriate directory to save outputs already exists.
#' dir.check automatically does this process
#' by just passing filename(s) you are going to save subsequently.
#' Thus the basic functionality is almost the same to
#' \code{dir.create(basename(x))},
#' but dir.check accepts multiple filenames
#' and performs some additional path handling.
#'
#' @param filename Strings. The name of the files you want to save
#'   (and thus of whose parent directories you want to create if needed).
#'
#' @return Strings.
#'   Names of the directories that was actually created.
#'
#' @examples
#'   \dontrun{
#'   f <- "test.eps"
#'   dir.check(f)
#'   eps(f, width = 6, height = 4)
#'   example(htb.colors)
#'   dev.off()
#'   }
#'
#' @keywords utilities
#'
#' @export

dir.check <- function(

	filename

) {

filename <- gsub(pattern = "\\\\", replacement = "/", x = filename)
filename <- path.expand(filename)

i <- !grepl("/$", filename)
filename[i] <- dirname(filename)[i]

filename <- filename[!file.exists(filename)]
if (length(filename) > 0) {
	sapply(X = filename, FUN = dir.create, recursive = TRUE)
}

invisible(filename)
}

