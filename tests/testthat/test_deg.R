context("Angular Value Handling")
library(htb)

d <- c(-180, -90, 0, 45, 180, 360)

test_that("wrapdeg() returns right-open degrees by default", {
	expect_equal(wrapdeg(d, 0), c(180, 270, 0, 45, 180, 0))
	expect_equal(wrapdeg(d, -180), c(-180, -90, 0, 45, -180, 0))
})

test_that("Minimal value is excluded from the left-open range", {
	expect_equal(wrapdeg(d, 0, leftopen = TRUE), c(180, 270, 360, 45, 180, 360))
	expect_equal(wrapdeg(d, -180, leftopen = TRUE), c(180, -90, 0, 45, 180, 0))
})

r <- c(-pi, 0, pi, 2 * pi)

test_that("wrapdeg() also accepts radian values", {
	expect_equal(wrapdeg(r, rad = TRUE), c(pi, 0, pi, 0))
})

