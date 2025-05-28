test_that("setCache changes cache", {
    bedbase <- BEDbase(quietly = TRUE)
    path <- tempdir()
    expect_true(BiocFileCache::bfccache(getCache(bedbase, "bedfiles")) != file.path(path, "bedfiles"))
    bedbase <- setCache(bedbase, path)
    expect_true(BiocFileCache::bfccache(getCache(bedbase, "bedfiles")) == file.path(path, "bedfiles"))
})

test_that("bb_example has data_format of 'bed' given rec_type 'bed'", {
    ex_bed <- bb_example(BEDbase(quietly = TRUE), "bed")
    expect_equal("bed", ex_bed$data_format)
})

test_that("bb_example has 'bed_ids' given rec_type 'bedset'", {
    ex_bed <- bb_example(BEDbase(quietly = TRUE), "bedset")
    expect_true("bed_ids" %in% names(ex_bed))
})

test_that("bb_metadata returns metadata for BEDs", {
    bedbase <- BEDbase(quietly = TRUE)
    ex_bed <- bb_example(bedbase, "bed")
    ex_metadata <- httr::content(bedbase$get_bed_metadata_v1_bed__bed_id__metadata_get(ex_bed$id, TRUE))
    bed_metadata <- bb_metadata(bedbase, ex_bed$id, TRUE)
    expect_identical(ex_metadata, bed_metadata)
})

test_that("bb_metadata returns metadata for BEDsets", {
    bedbase <- BEDbase(quietly = TRUE)
    ex_bedset <- bb_example(bedbase, "bedset")
    ex_bedset_metadata <- httr::content(bedbase$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(ex_bedset$id, TRUE))
    bedset_metadata <- bb_metadata(bedbase, ex_bedset$id, TRUE)
    expect_identical(ex_bedset_metadata, bedset_metadata)
})

test_that("bb_metadata errors on invalid input", {
    bedbase <- BEDbase(quietly = TRUE)
    expect_error(bb_metadata(bedbase, "invalid_invalid_invalid_invalid_", "bed"))
})

test_that("bb_list_beds returns same number of results for the hg19 genome", {
    bedbase <- BEDbase(quietly = TRUE)
    hg19_beds_raw <- httr::content(bedbase$list_beds_v1_bed_list_get(genome = "hg19"))
    hg19_bed_names <- unlist(lapply(hg19_beds_raw$results, `[`, c("name")),
        use.names = FALSE
    )
    hg19_beds <- bb_list_beds(bedbase, genome = "hg19")
    expect_equal(hg19_bed_names, hg19_beds$name)
    expect_true(all(hg19_beds$genome_alias == "hg19"))
})

test_that("bb_list_bedsets returns same number for query hg19", {
    bedbase <- BEDbase(quietly = TRUE)
    bedsets_raw <- httr::content(bedbase$list_bedsets_v1_bedset_list_get(query = "hg19"))
    bedsets_names_bed_ids_list <- lapply(bedsets_raw$results, `[`, c("id", "bed_ids"))
    bedsets_names_bed_ids <- dplyr::bind_rows(bedsets_names_bed_ids_list) |>
        tidyr::unnest(cols = c(bed_ids))
    bedsets <- bb_list_bedsets(bedbase, query = "hg19")
    expect_equal(bedsets_names_bed_ids$bed_ids, bedsets$bed_ids)
})

test_that("bb_list_bedsets returns number of bed ids", {
    bedbase <- BEDbase(quietly = TRUE)
    bedsets_raw <- httr::content(bedbase$list_bedsets_v1_bedset_list_get(limit = 1))
    bedsets_names_bed_ids_list <- lapply(bedsets_raw$results, `[`, c("id", "bed_ids"))
    bedsets_names_bed_ids <- dplyr::bind_rows(bedsets_names_bed_ids_list) |>
        tidyr::unnest(cols = c(bed_ids))
    bedsets <- bb_list_bedsets(bedbase, limit = 1)
    expect_equal(bedsets$bed_ids, bedsets_names_bed_ids$bed_ids)
})

test_that("bb_beds_in_bedset returns expected bed_ids", {
    bedbase <- BEDbase(quietly = TRUE)
    ex_bedset <- httr::content(bedbase$get_example_bedset_record_v1_bedset_example_get())
    ex_bedset_raw <- httr::content(bedbase$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(ex_bedset$id))
    ex_bed_ids_list <- lapply(ex_bedset_raw$results, `[`, c("id"))
    ex_bed_ids <- unlist(ex_bed_ids_list, use.names = FALSE)
    bed_ids <- bb_beds_in_bedset(bedbase, ex_bedset$id)$id
    expect_equal(ex_bed_ids, bed_ids)
})

test_that("bb_bed_text_search returns results scored against the query", {
    bedbase <- BEDbase(quietly = TRUE)
    beds <- bb_bed_text_search(bedbase, "hg38")
    ex_beds <- httr::content(bedbase$text_to_bed_search_v1_bed_search_text_post(
        query = "hg38",
        limit = 10,
        offset = 0
    ))
    ex_beds <- purrr::map_depth(.x = ex_beds$results, 1, \(y) unlist(y)) |>
        dplyr::bind_rows()
    expect_equal(ex_beds, beds)
    expect_true("score" %in% names(beds))
})

test_that("bb_to_granges returns a GRanges object given a 3+0 bed file", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    id <- "95a593b8337074a334b425aba5e77d4c"
    md <- bb_metadata(bedbase, id, TRUE)
    expect_equal("bed3+0", md$bed_compliance)
    gro <- bb_to_granges(bedbase, id)
    expect_true(methods::is((gro)[1], "GRanges"))
})

#test_that("bb_to_granges returns a GRanges object given a bigbed file", {
#    bedbase <- BEDbase(tempdir(), quietly = TRUE)
#    ex_bed <- bb_example(BEDbase(quietly = TRUE), "bed")
#    if (.Platform$OS.type != "windows") {
#        gro <- bb_to_granges(bedbase, ex_bed$id, "bigbed")
#        expect_true(methods::is((gro)[1], "GRanges"))
#    } else {
#        expect_warning(
#            rlang::warn("This feature does not work on Windows."),
#            bb_to_granges(bedbase, ex_bed$id, "bigbed")
#        )
#    }
#})

test_that("bb_to_granges returns a GRanges object given narrowpeak (6+4) file", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    md <- bb_metadata(bedbase, id, TRUE)
    expect_equal("bed6+4", md$bed_compliance)
    gro <- bb_to_granges(bedbase, id)
    expect_true(methods::is((gro)[1], "GRanges"))
    df <- as.data.frame(gro)
    expect_contains(
        c(
            "seqnames", "start", "end", "width", "strand", "name",
            "score", "signalValue", "pValue", "qValue", "peak"
        ),
        names(df)
    )
})

test_that("bb_to_granges returns GRanges object given bed3+9 with genome", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    id <- "608827efc82fcaa4b0bfc65f590ffef8"
    md <- bb_metadata(bedbase, id, TRUE)
    expect_equal("bed3+9", md$bed_compliance)
    gro <- bb_to_granges(bedbase, id)
    df <- as.data.frame(gro)
    expect_contains(
        c(
            "seqnames", "start", "end", "width", "strand", "name",
            "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12"
        ),
        names(df)
    )
})

test_that("bb_to_granges allows passing extra_cols", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    id <- "608827efc82fcaa4b0bfc65f590ffef8"
    md <- bb_metadata(bedbase, id, TRUE)
    expect_equal("bed3+9", md$bed_compliance)
    gro <- bb_to_granges(bedbase, id,
        extra_cols = c(
            "t1" = "character", "t2" = "character",
            "t3" = "character", "t4" = "character", "t5" = "character",
            "t6" = "character", "t7" = "character", "t8" = "character",
            "t9" = "character"
        )
    )
    expect_in(c(
        "seqnames", "start", "end", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
        "t8", "t9"
    ), names(as.data.frame(gro)))
})

test_that("bb_to_grangeslist creates a GRangesList", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    grl <- bb_to_grangeslist(bedbase, "lola_hg38_ucsc_features")
    expect_true(methods::is((grl)[1], "CompressedGRangesList"))
    expect_equal(11, length(grl))
})

test_that("bb_save saves bed files to a path", {
    bedbase <- BEDbase(tempdir(), quietly = TRUE)
    path <- tempdir()
    if (!dir.exists(path)) {
        dir.create(path)
    }
    bed <- bb_example(bedbase, "bed")
    bb_save(bedbase, bed$id, path, quietly = TRUE)
    expect_true(file.exists(file.path(path, paste0(bed$id, ".bed.gz"))))
    bedset <- bb_metadata(bedbase, "lola_hg38_ucsc_features")
    bb_save(bedbase, bedset$id, path, quietly = TRUE)
    for (id in bedset$bed_ids) {
        expect_true(file.exists(file.path(path, paste0(id, ".bed.gz"))))
    }
})
