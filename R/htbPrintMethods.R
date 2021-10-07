print.htbHd <- function(x, ...) { utils::str(x, give.attr = FALSE) }
print.htbDb <- function(x, ...) { utils::str(x, give.attr = FALSE) }
print.htbRas <- function(x, ...) { utils::str(x$da, give.attr = FALSE) }
print.htbHis <- function(x, ...) { utils::str(x$da, give.attr = FALSE) }

