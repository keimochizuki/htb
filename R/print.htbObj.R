#' Printer for htb objects
#'
#' Prints the brief summary of htb objects.
#'
#' Because most classes in htb package are actually
#' list objects with predetermined complicated format,
#' printing these objects thoughtlessly on the console
#' can result in vast verbose output.
#' To avoid this, printing methods for htb objects
#' uses [utils::str()] to prevent printing raw values.
#'
#' @param x htb objects.
#' @param ... Other arguments received for a compatibility purpose
#'   but in fact ignored.
#'
#' @examples
#'   db <- as.htbDb(replicate(2, rnorm(1000), simplify = FALSE),
#'     type = "analog")
#'   print(db)
#'
#' @keywords print
#'
#' @name print.htbObj
#' @export

print.htbHd <- function(x, ...) { utils::str(x, give.attr = FALSE) }

#' @rdname print.htbObj
#' @export

print.htbDb <- function(x, ...) { utils::str(x, give.attr = FALSE) }

#' @rdname print.htbObj
#' @export

print.htbRas <- function(x, ...) { utils::str(x$da, give.attr = FALSE) }

#' @rdname print.htbObj
#' @export

print.htbHis <- function(x, ...) { utils::str(x$da, give.attr = FALSE) }

