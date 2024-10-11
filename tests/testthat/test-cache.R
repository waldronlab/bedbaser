test_that("BEDBASE_CACHE is used if set", {
    Sys.setenv("BEDBASE_CACHE" = tempdir())
    path <- Sys.getenv("BEDBASE_CACHE")
    api <- BEDbase()
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(api, id, "bed")
    bfc <- BiocFileCache::BiocFileCache(path)
    bfci <- BiocFileCache::bfcinfo(bfc)
    expect_equal(BiocFileCache::bfccache(bfc), path)
    md <- bb_metadata(api, id, TRUE)
    file_path <- .get_file(md, "bed", "http")
    expect_true(paste0(file_path, ".gz") %in% bfci$rpath)
})

test_that("path is used if set when calling constructor", {
    path <- tempdir()
    bfc <- BiocFileCache::BiocFileCache(path)
    bfci <- BiocFileCache::bfcinfo(bfc)
    api <- BEDbase(path = path)
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(api, id, "bed")
    expect_equal(BiocFileCache::bfccache(bfc), path)
    md <- bb_metadata(api, id, TRUE)
    file_path <- .get_file(md, "bed", "http")
    expect_true(paste0(file_path, ".gz") %in% bfci$rpath)
})
