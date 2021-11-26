context("Plotting Utilities")
library(htb)

test_that("pval2lab() creates p value labels", {
	expect_match(pval2lab(NA), "p = NA")
	expect_match(pval2lab(0.9), "n\\.s\\.")
	expect_match(pval2lab(0.0777), "p = \\.078")
	expect_match(names(pval2lab(0.0777)), "+")
	expect_match(pval2lab(0.04), "p < \\.05")
	expect_match(names(pval2lab(0.04)), "\\*")
	expect_match(pval2lab(0.009), "p < \\.01")
	expect_match(names(pval2lab(0.00002)), "\\*\\*\\*")
})

