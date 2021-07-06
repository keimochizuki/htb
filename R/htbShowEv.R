htbShowEv <- function(

	db_event,
	newline,
	file = "",
	...

) {

if (class(db_event) != "htbDb") {
	stop("htbShowEv: Provided non-htbDb object.")
}

hd <- db_event$hd
info <- c(
	paste("# Protocol file      : ", hd$pro_file, sep = ""),
	paste("# Configuration file : ", hd$cfg_file, sep = ""),
	paste("# Experiment date    : ", hd$date, sep = ""))
info <- paste(info, collapse = "\n")
info <- paste(info, "\n", sep = "")
cat(info, file = file, ...)

ev <- db_event$ev
ev <- paste(ifelse(ev %in% newline, "\n", ""), ev, sep = "")
ev <- paste(ev, collapse = "\t")
ev <- paste(ev, "\n", sep = "")
cat(ev, file = file, ...)

invisible()
}

