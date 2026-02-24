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

test_that(".get_file returns a valid file path", {
    file_path <- .get_file(bedbase, ex_bed$id, tempdir())
    expect_true(file.exists(file_path))
    file_path <- .get_file(bedbase, ex_bed$id, getCache(bedbase, "bedfiles"))
    expect_true(file.exists(file_path))
})

test_that(".get_extra_cols returns a named vector", {
    ids <- ex_bedset$bed_ids[[1]]
    file_path <- .get_file(bedbase, ids, getCache(bedbase, "bedfiles"))
    ex_bed_md2 <- bb_metadata(bedbase, ids, TRUE)
    x_y <- strsplit(gsub("bed", "", ex_bed_md2$bed_compliance),
                    "+",
                    fixed= TRUE)[[1]]
    extra_cols <- .get_extra_cols(file_path,
                                  as.numeric(x_y[1]),
                                  as.numeric(x_y[2]))
    expect_equal(as.numeric(x_y[2]), length(extra_cols))
})
