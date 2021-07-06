htb.colors <- function(

	n,
	type = "rainbow"

) {

pal <- TRUE
if (type == "rainbow") {
	col <- switch(as.character(n),
		"1" = "#0072B2",
		"2" = c("#E69F00", "#0072B2"),
		"3" = c("#E69F00", "#56B4E9", "#0072B2"),
		"4" = c("#D55E00", "#E69F00", "#56B4E9", "#0072B2"),
		"5" = c("#D55E00", "#E69F00", "#D9CB00", "#56B4E9", "#0072B2"),
		"6" = c("#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2"),
		"7" = c("#E66080", "#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2"),
		"8" = c("#912CEE", "#E66080", "#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2"),
		"9" = c("#912CEE", "#E66080", "#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2", "#30308B"),
		c("#912CEE", "#E66080", "#e03040", "#D55E00", "#E69F00", "#D9CB00", "#009E73", "#56B4E9", "#0072B2", "#30308B"))
	pal <- n > 10
} else if (type == "bruna") {
	col <- c("#D95D2A", "#F8DF4A", "#1D508E", "#3C782E", "#845C40", "#726F62")
	if (n <= 6) {
		col <- col[1:n]
		pal <- FALSE
	}
} else if (type == "imas") {
	col <- c("#F70F1F", "#0775C4", "#AECECB", "#F29047",
		"#00A752", "#7E51A6", "#FA98BF", "#464B4F",
		"#FCD424", "#A1CA62", "#00B1BB", "#B51D66")
	if (n <= 12) {
		col <- col[1:n]
		pal <- FALSE
	}
}

if (pal) {
	col <- colorRampPalette(col)(n)
}

return(col)
}

