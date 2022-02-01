#' Creator for htbRas object
#'
#' Creates htbRas object from htbDb objects.
#'
#' In order to assess the function of recorded neural activity,
#' you need to examine the relationship between neuron's firing
#' and other experimental factors such as task events.
#' For example, some neurons may be activated at the time
#' of visual cue presentation, while others may fire
#' when the response is made.
#' For this analysis, your first step will be aligning
#' the neural activity by the occurrence of
#' a given task event.
#' In other words, you need to collect the activity of
#' the neuron around the onset of the targeted task event
#' for multiple times,
#' then you can create a rastergram or histogram for visually
#' assess the event-related activity of the neuron.
#'
#' [htbGetRas()] performs this data alignment process
#' for a given `htbDb` object.
#' A pair of htbDb objects with `spike` and `event` types
#' are used to create an aligned activity
#' packed into a dedicated list variable called `htbRas` object.
#' You can also use an `htbDb` object of `analog` type
#' when you want to align continuous data
#' (e.g., local field potential, electromyography, eye trajectory)
#' instead of intermittent spike timing data.
#' In either case, the `spike/analog` database and `event` database
#' must have been recorded in the same session
#' with completely identical storing configuration.
#' Otherwise, resulting *alignment* of the data does not make sense
#' since [htbGetRas()] has no capability in detecting
#' temporal offset or mismatch between provided databases.
#'
#' @param db_data An `htbDb` object of `spike` or `analog` type
#'   that contains the data you want to align.
#' @param db_event An `htbDb` object of `event` type
#'   used to align the data.
#' @param alignment A named list.
#'   Each element of the list must be a vector of length two,
#'   that designate the range of the extracted data
#'   around the time of aligning event in standard xlim style in R,
#'   i.e., `c(from, to)`.
#'   The name of the list is used as the name of the events to look at
#'   for the alignment.
#' @param incld A named list.
#'   The task event(s) needed to exist within an arbitrary range
#'   from the aligning event (designated by `alignment` argument).
#'   Format is the same to that of `alignment`,
#'   i.e., all the elements must be vectors of length two
#'   indicating `from` and `to` of the temporal range,
#'   and the name of the list corresponds to the events to be checked.
#'   This argument can be a list of such named lists,
#'   in which case they are used separately used for each element
#'   of `alignment`.
#' @param excld A named list.
#'   Same to `incld` but for task event(s) that *must not* exist
#'   within the range from aligning event.
#' @param cond A function.
#'   Conditional function (either [all()] or [any()]) used
#'   in in/exclusion process.
#'   When [all()] is used, a period of data is accepted
#'   and incorporated into resulting `htbRas` object
#'   if all the in/exclusion criteria designated by `incld` and `excld`
#'   are fulfilled.
#'   When [any()] is used, a period of data is appended
#'   if any one or more criteria of `incld` and `excld` are fulfilled.
#' @param ch Integer(s). The channel(s) of `db_data` to extract.
#'   When multiple channels are designated, [htbGetRas()]
#'   returns a list of `htbRas` objects (instead of an `htbRas` object)
#'   as a result of data extraction for each channel of the `htbDb` object
#'   with the same extraction settings.
#' @param event Strings.
#'   The names of task events you want to include in returning `htbRas` object.
#'   The timing information of these events can be then used
#'   in further visualization and analysis
#'   (e.g., plotting the event when you draw a rastergram).
#' @param title Strings.
#'   The titles for the extraction whose length
#'   equals to that of `alignment`.
#'
#' @return An `htbRas` object.
#'
#' @examples
#'   alignment <- list(CUEON_L = c(-1500, 2000), CUEON_R = c(-1500, 2000))
#'   incld <- list(TRIALSTART = c(-2000, 0), TRIALEND = c(0, 2000))
#'   excld <- list(ERROR = c(0, 2000))
#'   
#'   \dontrun{
#'   db_sp <- htbGetDb("spike.htb")
#'   db_ev <- htbGetDb("event.htb")
#'   ras <- htbGetRas(db_sp, db_ev, alignment,
#'     incld = incld, excld = excld)
#'   }
#'
#' @keywords utilities
#'
#' @export

htbGetRas <- function(

	db_data,
	db_event,
	alignment,
	incld = NULL,
	excld = NULL,
	cond = all,
	ch = NULL,
	event = NULL,
	title = NULL

) {

if (is.null(ch)) {
	ch <- which(!(names(db_data) %in% c("time", "hd")))
}
if (length(ch) > 1) {
	ras <- list()
	for (i in seq(along = ch)) {
		ras[[i]] <- htbGetRas(db_data = db_data, db_event = db_event,
			alignment = alignment, incld = incld, excld = excld,
			cond = cond, ch = ch[i], event = event)
	}
	return(ras)
}

is_named_xlims <- function(lim) {
	all(sapply(X = lim, FUN = function(z) {
		length(z) == 2 && is.vector(z, mode = "numeric")
	})) && !is.null(names(lim))
}
if (!is_named_xlims(alignment)) {
	stop("`alignment` must be a named list, all of whose elements being vectors of lengths two (in xlim style)")
}
if (!is.null(incld)) {
	if (is_named_xlims(incld)) {
		incld <- list(incld)
	}
	if (!all(sapply(X = incld, FUN = is_named_xlims))) {
		stop("`incld` must be a named list whose format being similar to `alignment` (or a list of such lists)")
	}
}
if (!is.null(excld)) {
	if (is_named_xlims(excld)) {
		excld <- list(excld)
	}
	if (!all(sapply(X = excld, FUN = is_named_xlims))) {
		stop("`excld` must be a named list whose format being similar to `alignment` (or a list of such lists)")
	}
}
n <- max(length(alignment), length(incld), length(excld))
alignment <- rep(alignment, length.out = n)
ev_align <- names(alignment)
incld <- suppressWarnings(rep(incld, length.out = n))
excld <- suppressWarnings(rep(excld, length.out = n))

if (is.null(title)) {
	title <- paste("cond", seq(along = alignment), sep = "")
}

param <- vector(length(alignment), mode = "list")
for (i in seq(along = alignment)) {
	param[[i]]$ev_align <- names(alignment)[i]
	param[[i]]$xlim <- alignment[[i]]
	param[[i]]$incld <- incld[[i]] # safe in case of NULL
	param[[i]]$excld <- excld[[i]] # safe in case of NULL
	param[[i]]$cond <- as.character(substitute(cond))
}
names(param) <- title

if (is.null(event)) {
	event <- sort(unique(db_event$ev))
}

da <- vector(length(param), mode = "list")
names(da) <- title
ev <- vector(length(event), mode = "list")
names(ev) <- event
check_ev <- function(
	ctp, # timepoint
	cev, # event
	clm  # lim
) {
	i <- (db_event$time >= (ctp + clm[1])) & (db_event$time <= (ctp + clm[2]))
	if (any(db_event$ev[i] == cev)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

for (i in seq(along = param)) {

	tmp <- param[[i]]
	xlims <- matrix(NA, nrow = 0, ncol = 2)

	rpt <- 0
	tmp_da <- list()
	tmp_ev <- vector(length(event), mode = "list")
	tmp_trg <- numeric()

	for (trig in db_event$time[db_event$ev == tmp$ev_align]) {

		checked <- logical(0)
		if (!is.null(tmp$incld)) {
			checked <- mapply(FUN = check_ev, trig, names(tmp$incld), tmp$incld)
		}
		if (!is.null(tmp$excld)) {
			checked <- c(checked, !mapply(FUN = check_ev, trig, names(tmp$excld), tmp$excld))
		}
		if (!cond(checked)) {
			next
		}

		# for event-based xlim designation
		xlim <- sapply(X = 1:2, FUN = function(k) {
			znum <- suppressWarnings(as.numeric(tmp$xlim[k]))
			if (!is.na(znum)) {
				return(znum)
			} else {
				l <- which(db_event$ev == tmp$xlim[k])
				if (length(l) == 0) {
					return(NA)
				}
				if (k == 1) {
					l <- l[max(which(db_event$time[l] < trig))]
				} else if (k == 2) {
					l <- l[min(which(db_event$time[l] > trig))]
				}
				if (length(l) == 0) {
					return(NA)
				} else {
					return(db_event$time[l] - trig)
				}
			}
		})
		if (any(is.na(xlim))) {
			next
		}
		fromto <- xlim + trig
		if (fromto[1] <= 0) {
			next
		}
		if ((db_data$hd$type == "analog") && (fromto[2] > length(db_data[[ch]]))) {
			next
		}

		rpt <- rpt + 1
		if (db_data$hd$type == "spike") {
			tmp_da[[rpt]] <- db_data[[ch]][(db_data[[ch]] >= fromto[1]) & (db_data[[ch]] <= fromto[2])] - trig
		} else if (db_data$hd$type == "analog") {
			tmp_da[[rpt]] <- db_data[[ch]][fromto[1]:fromto[2]]
		}
		tmp_trg <- c(tmp_trg, trig)

		iev <- (db_event$time >= fromto[1]) & (db_event$time <= fromto[2])
		evlist <- list(ev = db_event$ev[iev], time = db_event$time[iev] - trig)
		for (l in seq(along = event)) {
			tmp_ev[[l]][rpt] <- evlist$time[evlist$ev == event[l]][1] # only first event in the range
		}

		xlims <- rbind(xlims, xlim)
	}
	names(tmp_da) <- tmp_trg
	if (nrow(xlims) == 0) {
		xlims <- tmp$xlim
	} else {
		# xlims <- c(max(xlims[, 1]), min(xlims[, 2]))
		xlims <- c(min(xlims[, 1]), max(xlims[, 2]))
	}
	param[[i]]$xlim <- xlims

	da[[i]] <- tmp_da
	for (j in seq(along=event)) {
		ev[[j]][i] <- list(tmp_ev[[j]])
	}
}

ev_exist <- !sapply(X = ev, FUN = function(x){ all(is.na(unlist(x))) })
event <- event[ev_exist]
ev <- ev[ev_exist]

names(ev) <- event
for (i in seq(along = ev)) {
	names(ev[[i]]) <- title
}

ras <- list(da = da, ev = ev, param = param, hd = db_data$hd)
class(ras) <- "htbRas"

return(ras)
}

