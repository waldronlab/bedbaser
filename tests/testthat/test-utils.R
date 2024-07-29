test_that(".format_metadata_files returns a tibble with a url column", {
    client <- BEDbase()
    ex_bed <- bb_example(client, "bed")
    ex_md <- bb_metadata(client, ex_bed$id, "bed", TRUE)
    mdf <- .format_metadata_files(ex_md$files)
    expect_equal("tbl_df", class(mdf)[1])
    expect_true("url" %in% names(mdf))
})

test_that(".get_file returns a valid file path", {
    client <- BEDbase()
    ex_bed <- bb_example(client, "bed")
    md <- bb_metadata(client, ex_bed$id, "bed", TRUE)
    file_path <- .get_file(md, "bed", "http")
    expect_true(file.exists(file_path))
})

test_that(".get_extra_cols returns a named vector", {
    client <- BEDbase()
    id <- "608827efc82fcaa4b0bfc65f590ffef8"
    md <- bb_metadata(client, id, "bed", TRUE)
    file_path <- .get_file(md, "bed", "http")
    extra_cols <- .get_extra_cols(file_path, 3, 9)
    expect_equal(9, length(extra_cols))
})