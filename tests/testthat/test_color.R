context("Color Management")
library(htb)

test_that("htb.colors() returns `n` colors", {
	pattern <- "^#[0-9a-fA-F]{6}$"
	expect_match(htb.colors(1), pattern)
	expect_match(htb.colors(5), pattern)
	expect_match(htb.colors(30), pattern)
})

test_that("htb.colors() returns character(0) when designated <= 0", {
	expect_equal(htb.colors(0), character(0))
	expect_equal(htb.colors(-1), character(0))
	expect_error(htb.colors(5, type = "none_existing_type"))
})

