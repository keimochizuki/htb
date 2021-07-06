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

if (!all(sapply(X = alignment, FUN = length) == 2)) {
	stop("htbGetRas: All elements of <alignment> must have lengths of two (in an xlim style).")
}
if (!is.null(incld)) {
	if (all(sapply(X = incld, FUN = function(z) { (length(z) == 2) && !is.list(z) }))) {
		incld <- list(incld)
	}
}
if (!is.null(excld)) {
	if (all(sapply(X = excld, FUN = function(z) { (length(z) == 2) && !is.list(z) }))) {
		excld <- list(excld)
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
	param[[i]]$incld <- incld[[i]]
	param[[i]]$excld <- excld[[i]]
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
checkEv <- function(
	ctp, # timepoint
	cev, # event
	clm  # lim
) {
	o <- (db_event$time >= (ctp + clm[1])) & (db_event$time <= (ctp + clm[2]))
	if (any(db_event$ev[o] == cev)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

for (i in seq(along = param)) {

	tmp <- param[[i]]
	l_da <- length(db_data[[ch]])
	xlims <- matrix(NA, nrow=0, ncol=2)

	rpt <- 0
	tmpDa <- list()
	tmpEv <- vector(length(event), mode = "list")
	tmpTrg <- numeric()

	for (trig in db_event$time[db_event$ev == tmp$ev_align]) {

		checked <- logical(0)
		if (!is.null(tmp$incld)) {
			checked <- mapply(FUN = checkEv, trig, names(tmp$incld), tmp$incld)
		}
		if (!is.null(tmp$excld)) {
			checked <- c(checked, !mapply(FUN = checkEv, trig, names(tmp$excld), tmp$excld))
		}
		if (!cond(checked)) {
			next
		}

		# for event-based xlim designation
		xlim <- sapply(X = 1:2, FUN=function(k) {
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
		if ((db_data$hd$type == "analog") && (fromto[2] > l_da)) {
			next
		}

		rpt <- rpt + 1
		if (db_data$hd$type == "spike") {
			tmpDa[[rpt]] <- db_data[[ch]][(db_data[[ch]] >= fromto[1]) & (db_data[[ch]] <= fromto[2])] - trig
		} else if (db_data$hd$type == "analog") {
			tmpDa[[rpt]] <- db_data[[ch]][fromto[1]:fromto[2]]
		}
		tmpTrg <- c(tmpTrg, trig)

		iev <- (db_event$time >= fromto[1]) & (db_event$time <= fromto[2])
		evlist <- list(ev = db_event$ev[iev], time = db_event$time[iev] - trig)
		for (l in seq(along = event)) {
			tmpEv[[l]][rpt] <- evlist$time[evlist$ev == event[l]][1]
		}

		xlims <- rbind(xlims, xlim)
	}
	names(tmpDa) <- tmpTrg
	if (nrow(xlims) == 0) {
		xlims <- tmp$xlim
	} else {
		# xlims <- c(max(xlims[, 1]), min(xlims[, 2]))
		xlims <- c(min(xlims[, 1]), max(xlims[, 2]))
	}
	param[[i]]$xlim <- xlims

	da[[i]] <- tmpDa
	for (j in seq(along=event)) {
		ev[[j]][i] <- list(tmpEv[[j]])
	}
}

noevent <- sapply(X = ev, FUN = function(x){ all(is.na(unlist(x))) })
event <- event[!noevent]
ev <- ev[!noevent]

names(ev) <- event
for (i in seq(along = ev)) {
	names(ev[[i]]) <- title
}

ras <- list(da = da, ev = ev, param = param, hd = db_data$hd)
class(ras) <- "htbRas"

return(ras)
}

