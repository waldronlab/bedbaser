client <- BEDbase()

test_that(".format_metadata_files returns a tibble with a url column", {
    ex_bed <- bb_example(client, "bed")
    ex_md <- bb_metadata(client, ex_bed$id, "bed", TRUE)
    mdf <- .format_metadata_files(ex_md$files)
    expect_equal("tbl_df", class(mdf)[1])
    expect_true("url" %in% names(mdf))
})

test_that(".get_file_path returns a valid file path", {
    ex_bed <- bb_example(client, "bed")
    md <- bb_metadata(client, ex_bed$id, "bed", TRUE)
    file_url <- md$files$bed_file$access_methods[[1]]$access_url$url
    file_path <- .get_file_path(file_url, "bed")
    expect_true(file.exists(file_path))
})

test_that(".get_extra_cols returns a named vector", {
    id <- "608827efc82fcaa4b0bfc65f590ffef8"
    md <- bb_metadata(client, id, "bed", TRUE)
    file_path <- .get_file_path(md$files$bed_file$access_methods[[1]]$access_url$url,
                                "bed")
    extra_cols <- .get_extra_cols(file_path, 3, 9)
    expect_equal(9, length(extra_cols))
})