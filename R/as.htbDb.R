#' Constructor of an empty/artificial htbDb object
#'
#' Constructs an empty/artificial htbDb object with designated type.
#'
#' The readout from an .htb file is a list of htbDb objects
#' which is the most primitive data in TEMPO system.
#' Normally you will continue analyzing those extracted data
#' obtained in your own real-world experiment.
#' However, there can be times that you want to test your analyses
#' using artificially constructed spike, event and/or analog data.
#' This way you can check whether your script is working correctly or not
#' using virtual data with known properties
#' (e.g., time locking to some events, difference in firing rates, etc...).
#'
#' as.htbDb offers an easy way for this kind of
#' artificial data construction.
#' It creates an htbDb object with empty or
#' designated artificial data contents.
#' Because htbDb objects is actually a list with predetermined
#' properties and naming rules,
#' as.htbDb receives a list as an input object,
#' and converts it to an htbDb object.
#' Each element of the input list is regarded as
#' separate channels of constructed htbDb object
#' (i.e., spike timings for spike database,
#' codes of events for event database,
#' and analog integer values for analog database).
#'
#' Note that for event databases,
#' normal htbGetDb function performs data transformation
#' using character pattern matching (see Details in htbGetDb).
#' In as.htbDb, however, just pass event names (a vector of strings)
#' directly in the first element of the input list.
#' The second element is regarded as
#' channel with timing information.
#'
#' @param db A list. The dataset to be converted into an htbDb object.
#' @param type A string. The type of the resulting htbDb object
#'   (either one of \dQuote{spike}, \dQuote{event} or \dQuote{analog}).
#'
#' @return An htbDb object.
#'   Note that normal readout from htbGetDb function is
#'   a list of htbDb objects, because an htb file can
#'   contain multiple databases at once.
#'   On the other hand, as.htbDb returns just an htbDb ojbect
#'   (not a list of those objects).
#'   Therefore, some kind of data nesting and/or nest unpacking
#'   may be required to use constructed htbDb object(s)
#'   in your testing.
#'
#' @examples
#'   db_spike <- as.htbDb(replicate(3, {
#'     cumsum(ceiling(runif(100, 10, 50))) },
#'     simplify = FALSE),
#'     type = "spike")
#'
#'   db_event <- as.htbDb(list(
#'     c("TRL_START", "EV1", "EV2", "REWARD", "TRL_END"),
#'     time = cumsum(ceiling(runif(5, 100, 300))),
#'     type = "event")
#'
#'   db_spike <- as.htbDb(replicate(2, {
#'     cumsum(ceiling(rnorm(2000, sd = 4))) },
#'     simplify = FALSE), type = "analog")
#'
#' @export

as.htbDb <- function(

	db,
	type

) {

if (type == "event" && length(db) != 2) {
	stop("as.htbDb: <db> must have length of two for event database.")
}

hd <- list(
	date = "Converted from non-htb file",
	ldate = integer(),
	cfg_file = "",
	pro_file = "",
	speed = integer(),
	alloc = 0L,
	offset = 0L,
	period = integer(),
	extension = integer(),
	skip = integer(),
	first_channel = 0L,
	nchannels = length(db) - (type == "event"),
	sweep_limit = integer(),
	cancel_override = integer(),
	func = integer(),
	tag = integer(),
	npages = integer(),
	nsamples = length(db[[1]]),
	samples_per_page = integer(),
	sweep = integer(),
	next_page = integer(),
	next_off = integer(),
	title = "",
	speed_units = integer(),
	fileoffset = numeric(),
	filename = "",
	type = type,
	madeon = as.character(Sys.Date()))

if (type == "event") {
	names(db) <- c("ev", "time")
} else {
	names(db) <- paste("ch", seq(along = db), sep="")
}
db$hd <- hd
class(db) <- "htbDb"

invisible(db)
}

