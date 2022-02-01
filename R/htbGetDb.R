#' Extractor for htbDb object
#'
#' Extracts htbDb object from an htb file or existing RData file.
#'
#' TEMPO system may store several numbers of htb files
#' in one session of recording.
#' To read out the contents of these htb files,
#' you have to complete the following steps:
#' open up a connection to an htb file,
#' count the number of databases contained in it,
#' read the header information out,
#' extract the data from target database,
#' and finally close the connection.
#' (The definition of a database is subsequently given in detail.)
#' This may sound tiring for people with
#' less experience in programming.
#' In addition, the binary format of htb files
#' for spike and event data has one critical defect:
#' an extreme redundancy with massive zero-fillings.
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
#' This RData file will be used by [htbGetDb()]
#' next time you request the same data.
#' Therefore, after you have once extracted the data from
#' htb files with [htbGetDb()],
#' the time needed for the data extraction will be reduced
#' in the next time, because it actually does not reads the data from
#' htb files but from RData files.
#'
#' @section General organization of an htb file:
#' The format of htb files may seem a bit complicated,
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
#' In a given (one) htb file, a set of
#' multiple experimental data called *databases*
#' can be stored.
#' A database is a bundle of data with same data type,
#' which is one of the three possible types:
#' `spike`, `event` or `analog`.
#' As the names literally tell,
#' the type of a database indicate what type of information
#' is stored in that database.
#' Spike databases contain spike timing information.
#' Event databases contain timing information of certain events
#' in the experiment, as well as the codes (integer values)
#' to tell which event took place at each time point.
#' Analog databases contain continuous sequences of analog values
#' that originate from a transition of voltage loaded on A/D converter board.
#' Details of each data type are subsequently mentioned.
#'
#' The type of a database must be one of the three types above.
#' Other types are not accepted in TEMPO system,
#' and a database has one and only one data type from those three.
#' However, as mentioned before, an htb file can contain
#' multiple databases, and they can have different types.
#' This way TEMPO system can bundle up different types of
#' experimental data into one htb file.
#' Of course, you can also choose to devide each type of data
#' into different htb files as needed.
#'
#' A database, in turn, is composed of a MxN matrix,
#' where M is the number of data points,
#' and N is the number of data's input streams which are called *channels*.
#' A channel contains spiking of one isolated single neuron
#' in spike database,
#' and a voltage input from one input pin of the A/D board
#' in analog database.
#' Event database can also contain multiple channels.
#' For example, the first channel of an event database
#' is used to marker the timings of major events in the experiment
#' (e.g., trial start, response onset, reward delivery, etc...),
#' whereas the second channel stores additional information
#' about the observed events
#' (e.g., serial count of the trial, response time, amout of reward, etc...).
#' All of these information is saved as signed or unsigned
#' integer values whose bit rates (bit/sample) depend on
#' the subtype of the database.
#'
#' Taken together, an htb file can be composed of
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
#' @section An `htbDb` object:
#' Reading out such complicated (not so much in fact, though)
#' and nested .htb file is something bothersome
#' especially for newcomers.
#' Thus this package offers an easy solution
#' called [htbGetDb()] function.
#'
#' [htbGetDb()] opens a designated .htb file
#' and reads out all the contents of it,
#' regardless of how many and what types of databases
#' the file contains.
#' [htbGetDb()] first reads the file region called a `header`
#' (fixed lengths of bits that exist at the beginning of each database)
#' and automatically extract subsequent data region in a proper way.
#' Extracted data set from a database is concatenated into
#' a special list object called an `htbDb` object.
#'
#' Because an `htbDb` object corresponds to one database in an .htb file,
#' the contents of an `htbDb` object is one or more data channels of
#' either one of `spike`, `event` or `analog` type.
#' In whichever types, an `htbDb` object contains N elements
#' where N is the number of channels in the original data.
#' In `spike` type, each element of an `htbDb` object is an integer vector
#' that represents spike timings of each unit (nervous cell).
#' Since firing rates are different among recorded units,
#' the lengths of the vectors (numbers of spikes recorded from each unit)
#' can naturally differ.
#' In `analog` type, each element contains a series of integer values
#' that corresponds to an voltage input stream from the A/D board.
#' In one `analog` database, all the channels have the same sampling rates,
#' and are jointly manipulated to start/stop storing inputs during recording.
#' Thus the lengths of the elements (numbers of values recorded from each pin)
#' must be precisely identical in the case of `analog` database.
#'
#' In `event` type, things are a bit complicated.
#' Each element of an `event` database originally corresponds to
#' different event channel
#' defined in the TEMPO configuration.
#' (For your information, TEMPO system works according to
#' a *protocol* file with .pro extension and
#' a *protocol configuration* file with .pcf extension.
#' Configuration of databases is designated in the latter file.)
#' The number of channels contained in an `event` database is
#' completely arbitrary and up to the experimenter.
#' The critical thing here is, that TEMPO system can accept
#' only integer values for identification of task events.
#' Therefore, experimenters need to personally define
#' which value in which channel represents what event.
#' For example, imagine a case that you mainly use
#' the first channel of your `event` database.
#' Also imagine that you decided to use
#' a value of 1 to represent TRIALSTART,
#' 2 to CUEON,
#' 3 to CUEOFF,
#' 6 to RESPONSE,
#' and 10 to TRIALEND.
#' As mentioned above, the assignment of values to events
#' is completely arbitrary,
#' and missing numbers (e.g., 4, 5, 7, etc.) were just reserved untouched
#' for possible future use
#' (for example, change of the behavioral task and
#' addition of new task events).
#' For an explanatory simplification,
#' let call these integer values (e.g., 1) as expressions in event codes,
#' and corresponding literal labels (e.g., TRIALSTART)
#' as expressions in event names.
#'
#' As in the example above, you can store event information
#' into your `event` database only after
#' you establish name-to-code pairing rule.
#' Then, the information of codes (together with their timings)
#' are stored in .htb files.
#' However, as easy to imagine, expressions in event codes
#' are highly awkward since there is no actual necessity
#' to assign an integer value to an event.
#' To overcome this inconvenience,
#' htb package always require to re-translate event codes
#' into corresponding event names
#' when event data are read out
#' at the very first time using [htbGetDb()] function.
#'
#' For this, you need to declare the reversed code-to-name rule
#' to decode the event information.
#' This pairing rule needs to be passed to [htbGetDb()]
#' by `ecode` argument.
#' `ecode` is a full-path to an ascii text file,
#' which is a tab-separated 2-column table
#' with event codes (1st column) and names (2nd column).
#' Each row corresponds to a pair of event code and name.
#' Rows starting with a number sign (#) are omitted.
#' In the case of former example,
#' this `ecode` file should read as follows:
#' \tabular{rl}{
#'    1 \tab TRIALSTART \cr
#'    2 \tab CUEON      \cr
#'    3 \tab CUEOFF     \cr
#'    6 \tab RESPONSE   \cr
#'   10 \tab TRIALEND}
#' Beware of column separation with a tab.
#' This table informs [htbGetDb()] function about how to re-translate
#' event codes (in the original .htb file) back into
#' event names (in `htbDb` object and further used in your subsequent analyses).
#'
#' As a result, in `event` type,
#' the returned `htbDb` object is composed of two elements
#' called `ev` and `time`.
#' The former tells the event names occurred during recording,
#' and the latter tells their timing.
#' This allows you to further align your (`spike` and `analog`) data
#' based on the onsets of several task events
#' with htbGetRas() function.
#'
#' @section Details in `ecode` file:
#' We have just overviewed how you can designate
#' your own code-to-name pairing rule for `event` database.
#' When you are using only one channel for `event` database,
#' `ecode` file you need to create is simple and
#' essentially similar to the table shown above.
#' However, as previously mentioned, an `event` database can
#' be composed of multiple channels depending on your
#' TEMPO configuration.
#' In such a case, you need to use comma-separated glob expressions
#' in the first column of your `ecode` file.
#'
#' A `glob` is a simple wildcarding commonly used in programming language.
#' In short, an asterisk matches to 0 or more arbitrary character,
#' and a question mark matches to 1 arbitrary character.
#' For example, imagine that you have the same code-to-name rule
#' as above, but with a second extra channel.
#' If you don't actually care the contents of the second channel,
#' your `ecode` file should be like:
#' \tabular{rl}{
#'    1,* \tab TRIALSTART \cr
#'    2,* \tab CUEON      \cr
#'    3,* \tab CUEOFF     \cr
#'    6,* \tab RESPONSE   \cr
#'   10,* \tab TRIALEND}
#' You can see that the first column is composed of
#' comma-separated two values.
#' These correspond to the values of the first and second channel
#' needed for an event to be regarded to occur.
#' Because an asterisk matches any value,
#' this example means that you are actually ignoring
#' the second channel in your database.
#'
#' The genuine merit of this feature can be understood
#' (of course) when your second channel really carries information
#' about the task.
#' For example, imagine now your task have two different
#' (visual or auditory or whatever) cues at left and right,
#' as well as two separate left and right response keys.
#' You may want to use the same event code `2`
#' to indicate the presentation of the cue
#' (as in the examples we have used so far).
#' But you will also want to differentiate
#' whether the cue was presented on left or right side.
#' Now, you can utilize your second channel of the `event` database.
#' Because you have an additional space to hold information,
#' you can put (say) a value of `1` for left cue and
#' and `2` for right cue in the second channel.
#' In the same way, you can also put `1` or `2`
#' in the second channel when left or right key was pressed
#' at the time of response.
#' In this case, your `ecode` file will read:
#' \tabular{rl}{
#'    1,* \tab TRIALSTART \cr
#'    2,1 \tab CUEON_L    \cr
#'    2,2 \tab CUEON_R    \cr
#'    3,* \tab CUEOFF     \cr
#'    6,1 \tab RESPONSE_L \cr
#'    6,2 \tab RESPONSE_R \cr
#'   10,* \tab TRIALEND}
#' By virtue of the second channel of the database and glob matching,
#' you can classify different combinations of event codes in multiple channels
#' as events with different names.
#'
#' In `ecode` designation, partially duplicated globs
#' for event codes are accepted.
#' For example, even if you differentiate left and right cues,
#' you may, on the other hand, want to mark cue onsets
#' regardless of the side.
#' In such a case, you can designate like:
#' \tabular{rl}{
#'    2,* \tab CUEON_ANY \cr
#'    2,1 \tab CUEON_L   \cr
#'    2,2 \tab CUEON_R}
#' The first designation matches to both left and right cue,
#' while the second and third ones matches either one of left or right.
#' This results in both `CUEON_ANY` and `CUEON_L` events
#' to be marked at the onset of left cue,
#' and both `CUEON_ANY` and `CUEON_R` at the onset of right cue.
#' This may be sometimes handy when you want to first
#' check the gross data trends (e.g., neural activity)
#' at (either left or right) cue onsets,
#' and then go further to compare detailed conditional difference
#' (like left or right cue locations).
#' However, using the package's other functionalities
#' like htbCollapseCond(),
#' you can combine already-devided data groups into a merged one.
#' Thus you can also perform the same kind of analysis
#' by first marking task events in the most detailed way (like left or right),
#' and then combining them to see a gross trend.
#'
#' @param filename A string. The name of the htb file.
#'   The extension (.htb or .RData) can be omitted.
#' @param ecode A string. The (full-path) name of a text file to designate
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
				ec <- sub("^[[:blank:]]+", "", e[, 1])
				ec <- gsub("\\*", "[^,]*", ec)
				ec <- gsub("\\?", "[^,]", ec)
				ec <- paste("^", ec, "$", sep = "")

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

