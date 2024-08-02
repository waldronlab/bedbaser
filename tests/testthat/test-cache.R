test_that("bb_to_granges returns a GRanges object given narrowpeak (6+4) file", {
    Sys.setenv("BEDBASER_CACHE" = tempdir())
    cache_path <- Sys.getenv("BEDBASER_CACHE")
    api <- bedbaser()
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(api, id, "bed")
    bfc <- BiocFileCache::BiocFileCache(cache_path)
    bfci <- BiocFileCache::bfcinfo(bfc)
    expect_equal(bfccache(bfc), cache_path)
    md <- bb_metadata(api, id, TRUE)
    file_path <- .get_file(md, "bed", "http")
    expect_true(paste0(file_path, ".gz") %in% bfci$rpath)
})