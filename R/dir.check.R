dir.check <- function(

	filename

) {

filename <- gsub(pattern = "\\\\", replacement = "/", x = filename)

if (!grepl("/$", filename)) {
	filename <- dirname(filename)
}

if (!file.exists(filename)) {
	dir.create(filename, recursive = TRUE)
}

invisible(filename)
}

