bedbase <- BEDbase(tempdir(), quietly = TRUE)
ex_bed <- bb_example(bedbase, "bed")
ex_bedset <- bb_example(bedbase, "bedset")
ex_bed_md <- bb_metadata(bedbase, ex_bed$id, TRUE)

test_that(".get_file_name returns file name", {
    expect_equal(
        .get_file_name("https://this/is/an/example"),
        "example"
    )
})

test_that(".get_url returns a url", {
    file_url <- .get_url(ex_bed_md, "http")
    expect_true(stringr::str_detect(file_url, "(https?|ftp|s3)://"))
})

test_that(".get_file returns a valid file path", {
    file_path <- .get_file(ex_bed_md, tempdir(), "http")
    expect_true(file.exists(file_path))
    file_path <- .get_file(ex_bed_md, getCache(bedbase, "bedfiles"), "http")
    expect_true(file.exists(file_path))
})

test_that(".get_extra_cols returns a named vector", {
    ex_bed_md2 <- bb_metadata(bedbase, ex_bedset$bed_ids[[1]], TRUE)
    file_path <- .get_file(ex_bed_md2, getCache(bedbase, "bedfiles"), "http")
    extra_cols <- .get_extra_cols(file_path, 4, 1)
    expect_equal(1, length(extra_cols))
})
