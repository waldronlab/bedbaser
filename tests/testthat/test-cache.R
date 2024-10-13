test_that("default cache location is used", {
    path <- tools::R_user_dir("bedbaser", which = "cache")
    api <- BEDbase()
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(api, id, "bed")
    bfc <- BiocFileCache::BiocFileCache(path)
    expect_equal(BiocFileCache::bfccache(bfc), path)
    md <- bb_metadata(api, id, TRUE)
    file_path <- .get_file(md, "bed", "http", getCache(api))
    expect_true(paste0(file_path, ".gz") %in% BiocFileCache::bfcinfo(bfc)$rpath)
})

test_that("path is used if set when calling constructor", {
    path <- tempdir()
    api <- BEDbase(path)
    bfc <- getCache(api) 
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    gro <- bb_to_granges(api, id, "bed")
    expect_equal(BiocFileCache::bfccache(bfc), path)
    md <- bb_metadata(api, id, TRUE)
    file_path <- .get_file(md, "bed", "http", getCache(api))
    expect_true(paste0(file_path, ".gz") %in% BiocFileCache::bfcinfo(bfc)$rpath)
})
