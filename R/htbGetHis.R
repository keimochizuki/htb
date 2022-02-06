#' Creator for htbHis object
#'
#' Creates htbHis object from htbRas objects.
#'
#' In examining neural activities,
#' creating a histogram is one of the most elementary starting points.
#' A histogram visualizes the transition of instantaneous
#' firing rates of a neuron, calculated by
#' sequential averaging of number of spikes
#' with a sliding window of arbitrary width and steps.
#' For this, you need multiple lines of spike sequence
#' (normaly the trials in the task) aligned
#' at a time of certain external event.
#' Therefore, the source of a histogram is naturally
#' the data used to create a rastergram,
#' i.e., `htbRas` object in htb package.
#' [htbGetHis()] does this transformation for a set of spike sequences
#' packed as an `htbRas` object.
#' Result is returned as a specialized list named `htbHis` object,
#' containing a series of temporally smoothed firing rates
#' composed of the same set of task conditions with the source `htbRas` object.
#'
#' The major parameter of a histogram is the width and steps
#' of the sliding window for averaging.
#' These parameters are designated by `bin` and `sep` arguments, respectively.
#' If you use the same value (e.g., `100`) for both `bin` and `sep`,
#' it means that averaging windows adjoin each other without overlaps.
#' This is suitable for traditional histograms
#' that look like a lined skyscraper of buildings.
#' If you use small `sep` compared with `bin`,
#' it means that the sliding window moves in a smaller stride.
#' The windows overlap each other, but multiple usage (counting) of
#' the same spikes by neighboring windows does not matter
#' since the spike counts are anyway divided by the time width of the window.
#' Thus, this setting simply results in a smoothed version
#' of traditional histogram.
#' If you set larger value for `sep` than `bin`,
#' the windows get spaces in between.
#' This result in ignoring your precious spikes falled into these spaces,
#' and thus is normally no use.
#'
#' @param ras An `htbRas` object of `spike` type.
#'   You can provide a list of `htbRas` objects simultaneously,
#'   each of whose element is then used for histogram creation.
#'   In this case, [htbGetHis()]
#'   returns a list of `htbHis` objects (instead of an `htbHis` object).
#' @param xlim A pair of numerics.
#'   The range `c(from, to)` of time to calculate histograms.
#'   When omitted (default), the range of the original `htbRas` object
#'   (used in splicing spike sequence during alignment by [htbGetRas()])
#'   was inherited.
#' @param bin A numeric.
#'   The temporal width of the sliding window to calculate a histogram.
#' @param sep A numeric.
#'   The width of a step of the sliding window.
#'
#' @return An `htbHis` object.
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
#' his <- htbGetHis(ras)
#' }
#'
#' @keywords utilities
#'
#' @export

htbGetHis <- function(

	ras,
	xlim = NULL,
	bin = 200,
	sep = 10

) {

if ((class(ras) != "htbRas") && all(sapply(X = ras, FUN = class) == "htbRas")) {
	his <- list()
	for (i in seq(along=ras)) {
		his[[i]] <- htbGetHis(ras[[i]], xlim = xlim,
			bin = bin, sep = sep)
	}
	names(his) <- names(ras)
	return(his)
}

da <- ras$da
param <- ras$param
n <- length(da)

if (is.null(bin)) {
	bin <- sep
} else if (is.null(sep)) {
	sep <- bin
}

if (is.null(xlim)) {
	xlims <- sapply(X = param, FUN="[[", "xlim")
	xlim <- c(min(xlims[1,]), max(xlims[2,]))
}



y <- list()
s <- list()
if (attributes(ras)$type == "spike") {
	x <- seq(xlim[1], xlim[2], by = sep)
	for (i in seq(along = da)) {

		start <- pmax(x - bin / 2, xlim[1])
		end <- pmin(x + bin / 2, xlim[2])
		t_spike <- unlist(da[[i]])
		n_spike <- length(t_spike)
		n_trial <- length(da[[i]])
		if (n_trial != 0) {
			y[[i]] <- mapply(FUN = function(s, e) {
				sum((t_spike >= s) & (t_spike < e))
			}, start, end) / n_trial / (end - start) * 1000
		} else {
			y[[i]] <- rep(NA, length(x))
		}
		pars[[i]]$xlim <- xlim
		pars[[i]]$bin <- bin
		pars[[i]]$sep <- sep
		pars[[i]]$type <- "Firing Rate"
	}
	names(y) <- names(da)
	his <- list(da = list(x = x, y = y), param = param)

} else if (attributes(ras)$type == "analog") {
	x <- seq(xlim[1], xlim[2], by = 1)
	for (i in seq(along = da)) {
		if (length(da[[i]]) != 0) {
			tmp <- do.call(cbind, da[[i]])
			y[[i]] <- rowMeans(tmp)
			s[[i]] <- apply(X = tmp, MARGIN = 1, FUN = stats::sd)
		} else {
			y[[i]] <- rep(NA, length(x))
			s[[i]] <- rep(NA, length(x))
		}
		pars[[i]]$xlim <- xlim
		pars[[i]]$bin <- 1
		pars[[i]]$sep <- 1
		pars[[i]]$type <- "Analog Value"
	}
	names(y) <- names(da)
	names(s) <- names(da)
	his <- list(da = list(x = x, y = y, s = s), param = param)

}
class(his) <- "htbHis"

return(his)
}

