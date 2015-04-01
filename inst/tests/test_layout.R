context("Test `.GENOMIC_LAYOUT`")

test_that("Test `.GENOMIC_LAYOUT`", {
    
    pdf(NULL)
    gtrellis_layout(nrow = 2, n_track = 3)
    expect_that(gtrellis:::.GENOMIC_LAYOUT$fa,
        is_identical_to(paste0("chr", c(1:22, "X", "Y"))))

    expect_that(gtrellis:::.GENOMIC_LAYOUT$n_track, is_identical_to(3))
    expect_that(gtrellis:::.GENOMIC_LAYOUT$nrow, is_identical_to(2))
    expect_that(gtrellis:::.GENOMIC_LAYOUT$ncol, is_identical_to(12))
    
    dev.off()
})


df = data.frame(chr = c("chr1", "chr2"),
                start = c(1e8, 2e8),
                end = c(2e8, 3e8))

test_that("Test start position is not same", {

	pdf(NULL)
	expect_error(gtrellis_layout(df, ncol = 1), "start base in the same column should be the same")
	dev.off()

})
