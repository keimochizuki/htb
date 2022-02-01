context("Handling Objects")
library(htb)

db_sp <- htbGetDb("~/htb/wsdata", remake = TRUE)[[1]]
db_ev <- htbGetDb("~/htb/wedata", remake = TRUE, ecode = "~/htb/ecode")[[1]]
db_an <- htbGetDb("~/htb/wydata", remake = TRUE)[[1]]

alignment <- replicate(8, c(-1000, 2000), simplify = FALSE)
names(alignment) <- paste("E_CIDX", 1:8, sep = "")
incld <- list(E_REWARD = c(0, 6000))
ras <- htbGetRas(db_sp, db_ev,
	alignment = alignment,
	incld = incld,
	ch = 1)
#print(ras)

test_that("Extracting htbDb object", {
	expect_match(class(db_sp), "htbDb")
	expect_match(db_sp$hd$type, "spike")
	expect_match(db_ev$hd$type, "event")
	expect_match(db_an$hd$type, "analog")
})

test_that("Creating htbRas object", {
	expect_match(class(ras), "htbRas")
})

