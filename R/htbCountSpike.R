#' Calculator of spikes in htbRas object
#'
#' Counts number of spikes or firing rate an htbRas object.
#'
#' After you aligned and visualized neural activity
#' by creating an `htbRas` object,
#' you will then want to perform statistical tests
#' for the difference in firing between task conditions.
#' To count number of spikes in the `htbRas` object,
#' you can use [htbCountSpike()] function.
#' Because `htbRas` objects are already aligned by
#' certain task event (usually the most important
#' task event you are targeting),
#' you need to just designate the range of the period
#' by `from` and `to` arguments.
#' The spike counts within the range are calculated
#' for each of the conditions in the `htbRas` object.
#' The result is returned as a list,
#' whose length and numbers of elements contained
#' equal to the original number of conditions and numbers of trials
#' in the `htbRas` object, respectively.
#' [htbCountSpike()] can also return the firing rates
#' instead of the raw spike counts,
#' that may be more useful when you need to further
#' compare the neural activation between periods with different ranges
#' (and therefore this is set as the default behavior of the function).
#'
#' @param ras An `htbRas` object of `spike` type.
#'   Since "counting the number of firing" makes no sense,
#'   [htbCountSpike()] does not naturally accept `htbRas` object of `analog` type.
#' @param from A numeric or string.
#'   The start of the targeted period you want to examine in millisecond
#'   (relative to the aligning event you used in creating `htbRas` object).
#'   Alternatively, you can designate an event from where
#'   you want to start counting spikes.
#'   In the latter case, the designated event should always exists
#'   in (ideally) all the trials.
#'   Otherwise you will lose some trials, since the start of the period
#'   cannot be determined in trials in which that event did not happen.
#' @param to A numeric or string.
#'   Same to `from` argument but for the end of the targeted period.
#'   An event name can also be accepted.
#' @param type A string either `m` or `n`.
#'   If set `m` (default), [htbCountSpike()] returns firing rates
#'   by dividing the numbers of spikes by the lengths of designated period.
#'   If set `n`, [htbCountSpike()] returns raw spike counts instead.
#'
#' @return A list. Each element contains
#'   the spike counts or firing rates of the corresponding condition
#'   in the provided `htbRas` object.
#'
#' @examples
#' alignment <- list(CUEON_L = c(-1500, 2000), CUEON_R = c(-1500, 2000))
#' incld <- list(TRIALSTART = c(-2000, 0), TRIALEND = c(0, 2000))
#' excld <- list(ERROR = c(0, 2000))
#' 
#' \dontrun{
#' db_sp <- htbGetDb("spike.htb")
#' db_ev <- htbGetDb("event.htb")
#' ras <- htbGetRas(db_sp, db_ev, alignment,
#'   incld = incld, excld = excld)
#' s <- htbCountSpike(ras, 0, 500)
#' }
#'
#' @keywords math
#'
#' @export

htbCountSpike <- function(

	ras,
	from = NULL,
	to = NULL,
	type = "m"

) {

n <- sapply(X = ras$da, FUN = length)
for (i in c("from", "to")) {
	if (is.null(get(i))) {
		assign(i, mapply(FUN = function(z1, z2) {
			rep(z1$xlim[ifelse(i == "from", 1, 2)], z2)
		}, ras$param, n, SIMPLIFY = FALSE))
	} else if (is.character(get(i))) {
		assign(i, ras$ev[[get(i)]])
		if (is.null(get(i))) {
			stop("Non-existing event assigned as an interval")
		}
	} else {
		assign(i, lapply(X = n, FUN = rep, x = get(i)))
	}
}
l <- mapply(FUN = function(z1, z2) { z2 - z1 }, from, to, SIMPLIFY = FALSE)

s <- mapply(FUN = function(ss, fs, ts) {
	if (length(ss) == 0) {
		return(NULL)
	} else {
		return(mapply(FUN = function(x, y, z) {
			sum((x >= y) & (x < z)) }, ss, fs, ts))
	}
}, ras$da, from, to, SIMPLIFY = FALSE)

if (type == "m") {
	s <- mapply(FUN = function(x, y) { x / y * 1000 }, s, l, SIMPLIFY = FALSE)
}

return(s)
}

