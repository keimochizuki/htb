#' Extractor for htbDb object
#'
#' Extracts htbDb object from an htb file or existing RData file.
#'
#' TEMPO system may store several numbers of htb files
#' in one session of recording.
#' To read out the contents of these htb files,
#' you have to complete the following steps:
#' open up a connection to an htb file,
#' count the number of database contained in it,
#' read the header information out,
#' extract the data,
#' and finally close the connection.
#' This may sound tiring for people with
#' less experience in programming.
#' In addition, the binary format of htb files for spike and event
#' data is extremely redundant with massive zero-fillings.
#' It causes a long waiting time when you want to
#' extract the contents of htb files upon every analysis.
#'
#' [htbGetDb()] provides an easy way to carry out these
#' bothersome data extraction.
#' All you have to do is just designate the name of an htb file.
#' [htbGetDb()] extracts data from the designated htb file,
#' and creates `htbDb` objects.
#' This is the most basic form of data in htb package.
#' Each `htbDb` object is a list, containing the data
#' for all the channels in the original database.
#' (See the following section for detailed information about
#' the data format of htb files.)
#' Because an htb file can contain multiple databases,
#' the value returned by [htbGetDb()]
#' is a list of `htbDb` objects (instead of a single `htbDb` object).
#' This is always true even when only one database is extracted.
#' In such cases, a list of length one will be returned,
#' of whose the only content is an `htbDb` object.
#'
#' [htbGetDb()] automatically saves the `htbDb` objects as RData file,
#' in the same directory and as the same name to
#' the original htb file.
#' This RData file will be loaded and returned by [htbGetDb()]
#' next time you request the same data.
#' Therefore, after you have once extracted the data from
#' htb files with [htbGetDb()],
#' the time needed for the data extraction will be reduced
#' in the next time, because it actually does not reads the data from
#' htb files but from RData files.
#'
#' @section General organization of an htb file:
#' The format of htb files may seem to be a bit complicated,
#' especially for those unfamiliar to TEMPO system.
#' The author personally does not think that
#' the deep understanding on these formats is mandatory for everyone.
#' However, a basic knowledge about how neuronal activities
#' are stored in data files would anyway help,
#' regardless of whether you use actual TEMPO system or not.
#' Thus here I briefly mention the very basic of htb files,
#' and how they are handled in this package.
#' If you need more detailed information,
#' you can learn the binary format of htb files in TEMPO's reference manual.
#'
#' First of all, the unit of information stored with TEMPO system
#' is an htb file, a binary file with .htb extension.
#' In a given htb file, there can be a set of
#' multiple experimental data called *databases*.
#' A database is a bundle of data with same data type,
#' which is one of the three possible types:
#' `spike`, `event` or `analog`.
#' As the names literally tell,
#' the type of a database indicate what type of information
#' is stored in that database.
#' Spike databases contain spike timing information.
#' Event databases contain timing information of certain events
#' in the experiment, as well as the codes to show
#' which event took place.
#' Analog databases contain continuous sequence of analog values,
#' recorded as a transition of voltage loaded on A/D converter board.
#' Details of each data type are subsequently mentioned.
#'
#' The type of a database must be one of the three types above.
#' Other types are not accepted in TEMPO system,
#' and a database has one and only one data type from those three.
#' However, as mentioned before, an htb file can contain
#' multiple databases, and they can have different types.
#' This way TEMPO system can bundle up different types of
#' experimental data into one htb file.
#'
#' A database, in turn, is composed of a MxN matrix,
#' where M is the number of data points,
#' and N is the number of data's input streams which are called *channels*.
#' A channel contains spiking of one isolated single neuron
#' in spike database,
#' and voltage input from one A/D board's input pin
#' in analog database.
#' Event database can also contain multiple channels.
#' For example, the first channel of an event database
#' is used to marker the timings of major events in the experiment
#' (e.g., trial start, response, reward delivery, etc...),
#' whereas the second channel stores additional information
#' about the observed events
#' (e.g., serial number of the trial, response time, amout of reward, etc...).
#'
#' Taken together, an htb file may be composed of
#' multiple databases that can (but not necessarily need to) be
#' different types,
#' and each database may be composed of one or more channels.
#' For example, imagine that your experiment consists of
#' simultaneous recording of
#' up to 20 isolated spiking activities,
#' one event sequence for data alignment,
#' and two analog values that corresponds to the x and y coordinates
#' of the eye position.
#' Each of the three types of data must be stored in separate
#' databases with `spike`, `event` and `analog` data types,
#' but can be still held in one htb file.
#'
#' In addition, an `htbDb` object for event database
#' has another special channel named `time`.
#' Those types of database is converted to *time encoding*
#' forms by [htbGetDb()], so this `time` channel is necessary.
#' nuro nuro
#' aaa. aas.
#'
#' subsection megassa
#' bbs  a a.
#' bbs  a ccc
#'
#' @param filename A string. The name of the htb file.
#'   The extension (.htb or .RData) can be omitted.
#' @param ecode A string. The name of a text file to designate
#'   the correspondency between event codes and names.
#'   The file must be a tab-separated two-columnar table
#'   of which the first column contains glob expression of the
#'   event codes and the second column contains the corresponding
#'   event names.
#' @param remake A logical. Whether to re-extract `htbDb` object
#'   from an htb file even if an RData is available.
#'
#' @return A list of `htbDb` objects.
#'   The length of the list is the number of databases existed
#'   in the original htb file. (See Details.)
#'
#' @examples
#'   \dontrun{
#'   db <- htbGetDb("an_htb_file.htb")
#'   }
#'
#' @keywords IO
#'
#' @export

htbGetDb <- function(

	filename,
	ecode = NULL,
	remake = FALSE

) {

fn_htb <- path.expand(filename)
if (!grepl("\\.htb$", fn_htb, ignore.case = TRUE)) {
	fn_htb <- paste(fn_htb, ".htb", sep = "")
}
fn_rdata <- sub("^(.*)\\.htb$", "\\1\\.RData", fn_htb, ignore.case = TRUE)



cat("htbGetDb: ")
if (file.exists(fn_rdata) && !remake) {

	load(fn_rdata)
	cat("Loaded ", fn_rdata, "\n", sep="")
	utils::flush.console()

} else {

	fid <- file(description = fn_htb, open = "rb")
	on.exit({close(fid)})

	read <- function(at, type, n = 1) {
		readdata <- 0
		seek(fid, at, origin = "start")
		what <- ifelse(type == "STRING", "character", "int")
		size <- c(UCHAR = 1, CHAR = 1, USHORT = 2, SHORT = 2,
			ULONG = 4, LONG = 4, STRING = 1)[type]
		signed <- !(type %in% c("UCHAR", "USHORT"))
		return(readBin(fid, what = what, n = n, size = size, signed = signed))
	}

	# count dbs in the file (originally htbCount)
	current <- 0
	n_db <- 0
	repeat {
		nbytes <- read(current + 114, "ULONG")
		if (length(nbytes) == 0) {
			break
		}
		current <- current + nbytes
		n_db <- n_db + 1
	}
	seek(fid, 0, origin="start")

	db <- list()
	offset <- 0
	for (i in 1:n_db) {

		# read header information (originally htbGetHd)
		hd <- list(
			date = read(offset + 0, "STRING"),
			ldate = read(offset + 26, "LONG"),
			cfg_file = read(offset + 30, "STRING"),
			pro_file = read(offset + 44, "STRING"),
			speed = read(offset + 110, "ULONG"),
			alloc = read(offset + 114, "ULONG"),
			offset = read(offset + 118, "LONG"),
			period = read(offset + 122, "ULONG"),
			extension = read(offset + 126, "ULONG"),
			skip = read(offset + 130, "USHORT"),
			first_channel = read(offset + 132, "USHORT"),
			nchannels = read(offset + 134, "USHORT"),
			sweep_limit = read(offset + 136, "USHORT"),
			cancel_override = read(offset + 138, "ULONG"),
			func = read(offset + 142, "UCHAR"),
			tag = read(offset + 144, "USHORT"),
			npages = read(offset + 146, "USHORT"),
			nsamples = read(offset + 148, "ULONG"),
			samples_per_page = read(offset + 152, "USHORT"),
			sweep = read(offset + 154, "USHORT"),
			next_page = read(offset + 156, "USHORT"),
			next_off = read(offset + 158, "USHORT"),
			title = read(offset + 160, "STRING"),
			speed_units = read(offset + 240, "ULONG"),
			fileoffset = offset,
			filename = fn_htb)

		# 0: Analog average 8 bit
		# 1: Analog append  8 bit
		# 2: Spike  average 16 bit
		# 3: Spike  append  16 bit
		# 4: Event  average 16 bit
		# 5: Event  append  16 bit
		# 6: Analog average 16 bit
		# 7: Analog append  16 bit
		hd$type <- switch(as.character(hd$func),
			"0"="analog", "1"="analog", "2"="spike", "3"="spike",
			"4"="event", "5"="event", "6"="analog", "7"="analog", "unknown")
		hd$madeon <- as.character(Sys.Date())

		# read data (originally htbGetDa)
		type <- paste(ifelse(hd$type == "analog", "", "U"),
			ifelse(hd$func <= 1, "CHAR", "SHORT"), sep = "")
		n_epoch <- ifelse((hd$func %% 2) == 0, 1, hd$sweep) # only 1 epoch for average db
		n_col <- hd$nchannels
		n_row <- (hd$period) * n_epoch

		da <- read(hd$fileoffset + 512, type = type, n = n_col * n_row)
		l <- length(da)
		if ((l < (n_col * n_row)) && (l != 0)) {
			da <- c(da, rep(0, n_col * n_row - l))
			warning("htbGetDb: Data length (", l, ") unmatches to expected size (", n_row, " x ", n_col, ")")
		}
		if (hd$type == "spike") {
			da <- as.logical(da)
		}
		da <- matrix(da, nrow = n_row, ncol = n_col, byrow = TRUE)

		tmp  <- list()
		if (hd$type == "event") {
			time <- which(rowSums(da) != 0)
			da <- da[time,, drop = FALSE]
			codes <- apply(X = da, MARGIN = 1, FUN = paste, collapse = ",")
			if (is.null(ecode)) {
				tmp$ev <- codes
				tmp$time <- time
			} else {
				e <- utils::read.table(ecode, stringsAsFactors = FALSE,
					sep = "\t", comment.char = "#")
				ec <- paste("^", gsub("\\*", "[^,]+", e[, 1]), sep = "")

				et <- lapply(X = ec, FUN = function(z) {
					j <- grepl(z, codes)
					if (!any(j)) {
						return(integer(0))
					} else {
						return(time[j])
					}
				})
				en <- rep(e[, 2], sapply(X = et, FUN = length))
				et <- unlist(et)

				j <- order(et)
				tmp$ev <- en[j]
				tmp$time <- et[j]
			}
		} else {
			for (j in 1:ncol(da)) {
				ch <- paste("ch", j, sep = "")
				if ((hd$type == "spike") && any(da[, j])) {
					tmp[[ch]] <- which(da[, j])
				} else if (hd$type == "analog") {
					tmp[[ch]] <- da[, j]
				}
			}
		}
		tmp$hd <- hd
		class(tmp) <- "htbDb"
		db[[i]] <- tmp

		offset <- offset + hd$alloc
	}
	names(db) <- paste("db", seq(along = db), sep = "")
	cat("Extracted ", fn_htb, "\n", sep="")
	utils::flush.console()

	save(list = "db", file = fn_rdata)
}

invisible(db)
}

