#' Creator for animated movie graphics
#'
#' Creates a movie clip from sequential graphical outputs.
#'
#' When analyzing the data, there are often cases
#' you want to visualize your result in animated movie clips
#' rather than static graphics like eps or png.
#' [htbPlotAnim()] provides an easy way to such dynamic graphical outputs.
#' Since R intrinsically does not have built-in dynamic graphics,
#' this function achieves it by virtue of
#' external software such as `ffmpeg` or `convert (ImageMagic)`.
#' By just designing the size of the output
#' along with a set of R codes to plot multiple sequential static images,
#' [htbPlotAnim()] puts out those images as temporal files
#' and concatenates them into a movie clip.
#' Static images are then automatically removed,
#' thus no garbage files are left behind.
#'
#' If corresponding terminal command (`ffmpeg` for mp4 and `convert` for gif)
#' is not available,
#' [htbPlotAnim()] cannot compile a movie and prints an error.
#' In such a case, please install the required software
#' and make sure it is executable.
#' (Management of environmental variable may be needed for Windows users.)
#' Alternatively, you can explicitly designate a full path
#' to the converting command by `converter` argument.
#' Be careful, however, because
#' consistency between output file type and the converting command
#' is not verified in this case.
#'
#' @param filename A string. The name of the created movie file.
#'   The extension (.eps) can be omitted
#'   if valid file type is designamted by `type` argument.
#' @param width A numeric. The width of the device in pixels.
#' @param height A numeric. The height of the device in pixels.
#' @param expr An expression. R codes to plot a set of static images
#'   that are then converted in a movie file.
#'   Use braces (curly bracket) to enclose multiple lines of code
#'   without bothersome semicolons between commands.
#' @param type A string. The type of the animated output.
#'   Either `mp4` or `gif` is supported.
#'   (Support for `swf` is no more available
#'   due to the abandonment of Adobe Flash.)
#'   If appropriate extension is provided as `filename` argument,
#'   corresponding `type` is automatically selected.
#' @param img A string. The name of the output device for plotting.
#'   Either `png` (suitable for graphs)
#'   or `jpg` (suitable for picture-like images) is supported.
#' @param converter A string. The name of the command to compile
#'   static images into a movie.
#'   Either `ffmpeg` (for mp4 output) or `convert` (for animated gif output)
#'   is automatically selected.
#'   Therefore, you do not normally need to explicitly designate this argument.
#'   In case your terminal does not have an access
#'   to neither of these commands,
#'   you can designate a full path to the corresponding executable file
#'   as `converter`, which is then used during system calls in [htbPlotAnim()].
#' @param digit An integer. The number of digits reserved for
#'   plotting static frames.
#'   Default value `5` will be normally enough
#'   since this means you can use up to 99999 frames,
#'   i.e., almost 60-min movie in 30 fps.
#' @param kbps An integer. An approximate target bit rate
#'   for mp4 output passed to `ffmpeg`.
#'   Ignored when using `convert` for gif output.
#' @param fps An integer. The frame rate of the movie.
#' @param recursive A logical. Whether to make the animated gif file
#'   to be recursively looped or not.
#'   Ignored when using `ffmpeg` for mp4 output.
#' @param scale A numeric. Rescaling parameter used during
#'   creation of the movie from static images.
#'   Normally use `100` (default) if you have already sellected
#'   a desired size of the movie by `width` and `height`.
#'   In some cases, this argument might be useful
#'   if you wish to draw source images in high resolution
#'   but only need coarse movie (`scale < 100` with large `width` and `height`).
#'   Alternatively, you can also draw coarse images in low resolution
#'   but still make the movie output to be geometrically larger in size
#'   (`scale > 100` with small `width` and `height`).
#' @param transparent A string. The color recognized as a background
#'   and made to be transparent in animated gif output.
#'   If not set, all the images are used without transparency.
#'   Ignored when using `ffmpeg` for mp4 output.
#' @param verbose A logical. Whether to print detailed output
#'   from the converting command into R's terminal.
#' @param rmimgfiles A logical. Whether to remove static image files.
#'
#' @examples
#' \dontrun{
#' n <- 150
#' w0 <- cumsum(rnorm(n, mean = 0))
#' w1 <- cumsum(rnorm(n, mean = 0.2))
#' htbPlotAnim("randomwalk.mp4", 320, 240, {
#'   par(mar = c(3, 3, 1, 1), las = 1)
#'   for (i in 1:n) {
#'     j <- 1:i
#'     blankplot(xlim = c(0, i), ylim = range(c(w0[j], w1[j])))
#'     lines(j, w0[j], lty = "22")
#'     lines(j, w1[j], lwd = 2)
#'   }
#' }, fps = 15)
#' }
#'
#' @keywords device
#'
#' @export

htbPlotAnim <- function(

	filename,
	width,
	height,
	expr,
	type = c("mp4", "gif"),
	img = c("png", "jpg"),
	converter = NULL,
	digit = 5,
	kbps = "300k",
	fps = 10,
	recursive = TRUE,
	scale = 100,
	transparent = "",
	verbose = TRUE,
	rmimgfiles = TRUE

) {

if (grepl("\\.(mp4|gif)$", filename, ignore.case = TRUE)) {
	type <- tolower(substring(filename, nchar(filename) - 2))
} else {
	type <- type[1]
	filename <- paste(filename, type, sep = ".")
}
img <- img[1]

if (is.null(converter)) {
	converter <- switch(type, mp4 = "ffmpeg", gif = "convert")
}
if (system(paste(converter, "-version"), ignore.stdout = verbose, ignore.stderr = verbose) != 0) {
	stop(paste("Terminal command `", converter, "` not available", sep = ""))
}

tmp <- paste(tempfile(), "_", sep = "")
imgname <- paste(tmp, "%0", digit, "d.", img, sep = "")
glob <- paste(tmp, "*.", img, sep = "")
regx <- paste(basename(tmp), ".*\\.", img, sep = "")



cat("Exporting frames ")
utils::flush.console()

start <- Sys.time()
if (img == "png") {
	grDevices::png(imgname, width = width, height = height)
} else if (img == "jpg") {
	grDevices::jpeg(imgname, width = width, height = height)
}
eval(expr)
grDevices::dev.off()



cat("(", round(difftime(Sys.time(), start, units = "secs"), 1), " s) ... ", sep = "")
cat("Converting ")
utils::flush.console()
start <- Sys.time()
err <- try(
#if (type == "swf") {
#	system(paste("png2swf -r ", fps,
#		ifelse(scale != 100, paste(" -s ", scale, sep = ""), ""),
#		" -o ", filename, " ", glob, sep = ""))
#}
if (type == "mp4") {
	scale <- scale / 100
	system(paste(converter,
		" -y ",
		" -r ", fps,
		" -i ", imgname,
		#" -pix_fmt yuv420p",
		" -an -b:v ", kbps,
		" -r ", fps,
		ifelse(scale != 1, paste(" -s ", width * scale, "x", height * scale, sep = ""), ""),
		" ", filename, sep = ""),
		ignore.stdout = verbose, ignore.stderr = verbose)
} else if (type == "gif") {
	system(paste(converter,
		" -delay ", round(1000 / fps / 10),
		ifelse(scale != 100, paste(" -resize ", scale, "%", sep = ""), ""),
		ifelse(transparent != "", paste(" -transparent \"", transparent, "\" -dispose previous", sep = ""), ""),
		" -loop ", ifelse(recursive, "0", "1"),
		" ", glob, " ", filename, sep = ""),
		ignore.stdout = verbose, ignore.stderr = verbose)
})
cat("(", round(difftime(Sys.time(), start, units = "secs"), 1), " s) ... Done\n", sep = "")
utils::flush.console()

if (rmimgfiles) {
	f <- list.files(path = dirname(tmp), pattern = regx, full.names = TRUE)
	file.remove(f)
}

invisible()
}

