test_that("BEDBASER_CACHE is used if set", {
    Sys.setenv("BEDBASER_CACHE" = tempdir())
    cache_path <- Sys.getenv("BEDBASER_CACHE")
    api <- BEDbase()
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(api, id, "bed")
    bfc <- BiocFileCache::BiocFileCache(cache_path)
    bfci <- BiocFileCache::bfcinfo(bfc)
    expect_equal(BiocFileCache::bfccache(bfc), cache_path)
    md <- bb_metadata(api, id, TRUE)
    file_path <- .get_file(md, "bed", "http")
    expect_true(paste0(file_path, ".gz") %in% bfci$rpath)
})
