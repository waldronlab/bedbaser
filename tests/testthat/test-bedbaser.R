bedbase <- BEDbase(quietly = TRUE)
ex_bed <- bb_example(bedbase, "bed")
ex_bedset <- bb_example(bedbase, "bedset")

test_that("setCache changes cache", {
    path <- tempdir()
    expect_true(BiocFileCache::bfccache(getCache(bedbase, "bedfiles")) != file.path(path, "bedfiles"))
    bedbase <- setCache(bedbase, path)
    expect_true(BiocFileCache::bfccache(getCache(bedbase, "bedfiles")) == file.path(path, "bedfiles"))
})

test_that("bb_example has data_format given rec_type 'bed'", {
    expect_true("data_format" %in% names(ex_bed))
})

test_that("bb_example has 'bed_ids' given rec_type 'bedset'", {
    expect_true("bed_ids" %in% names(ex_bedset))
})

test_that("bb_metadata returns metadata for BEDs", {
    ex_metadata <- httr::content(bedbase$get_bed_metadata_v1_bed__bed_id__metadata_get(ex_bed$id, TRUE))
    bed_metadata <- bb_metadata(bedbase, ex_bed$id, TRUE)
    expect_identical(ex_metadata, bed_metadata)
})

test_that("bb_metadata returns metadata for BEDsets", {
    ex_bedset_metadata <- httr::content(bedbase$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(ex_bedset$id, TRUE))
    bedset_metadata <- bb_metadata(bedbase, ex_bedset$id, TRUE)
    expect_identical(ex_bedset_metadata, bedset_metadata)
})

test_that("bb_metadata errors on invalid input", {
    expect_error(bb_metadata(bedbase, "invalid_invalid_invalid_invalid_", "bed"))
})

test_that("bb_list_beds returns same number of results for the mm39 genome", {
    mm39_beds_raw <- httr::content(bedbase$list_beds_v1_bed_list_get(
        genome = "mm39",
        limit = 5000
    ))
    mm39_bed_names <- unlist(lapply(mm39_beds_raw$results, `[`, c("name")),
        use.names = FALSE
    )
    mm39_beds <- bb_list_beds(bedbase, genome = "mm39", limit = 5000)
    expect_equal(sort(mm39_bed_names), sort(mm39_beds$name))
    expect_true(all(mm39_beds$genome_alias == "mm39"))
})

test_that("bb_list_bedsets returns same number for query 'alzheimer'", {
    bedsets_raw <- httr::content(bedbase$list_bedsets_v1_bedset_list_get(
        query = "alzheimer",
        limit = 5000
    ))
    bedsets_names_bed_ids_list <- lapply(bedsets_raw$results, `[`, c("id", "bed_ids"))
    bedsets_from_raw <- dplyr::bind_rows(bedsets_names_bed_ids_list) |>
        tidyr::unnest(cols = c(bed_ids))
    bedset_ids <- bedsets_from_raw$id |> unique()
    bedsets <- bb_list_bedsets(bedbase, query = "alzheimer", limit = 5000)
    expect_equal(bedset_ids, bedsets$id)
})

test_that("bb_list_bedsets returns limited number of bed ids", {
    bedsets_raw <- httr::content(bedbase$list_bedsets_v1_bedset_list_get(
        query = "",
        limit = 1
    ))
    bedsets_names_bed_ids_list <- lapply(bedsets_raw$results, `[`, c("id", "bed_ids"))
    bedsets_from_raw <- dplyr::bind_rows(bedsets_names_bed_ids_list) |>
        tidyr::unnest(cols = c(bed_ids))
    bedset_ids <- bedsets_from_raw$id |> unique()
    bedsets <- bb_list_bedsets(bedbase, query = "", limit = 1)
    expect_equal(bedset_ids, bedsets$id)
})

test_that("bb_beds_in_bedset returns expected bed_ids", {
    ex_bedset <- httr::content(bedbase$get_example_bedset_record_v1_bedset_example_get())
    ex_bedset_raw <- httr::content(bedbase$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(ex_bedset$id))
    ex_bed_ids_list <- lapply(ex_bedset_raw$results, `[`, c("id"))
    ex_bed_ids <- unlist(ex_bed_ids_list, use.names = FALSE)
    bed_ids <- bb_beds_in_bedset(bedbase, ex_bedset$id)$id
    expect_equal(ex_bed_ids, bed_ids)
})

test_that("bb_bed_text_search returns results scored against the query", {
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
    beds <- bb_list_beds(bedbase, bed_compliance = "bed3+0")
    id <- beds$id[1]
    md <- bb_metadata(bedbase, id, TRUE)
    expect_equal("bed3+0", md$bed_type)
    gro <- bb_to_granges(bedbase, id)
    expect_true(methods::is((gro)[1], "GRanges"))
})

test_that("bb_to_granges returns a GRanges object given narrowpeak (6+4) file", {
    beds <- bb_list_beds(bedbase, bed_compliance = "bed6+4")
    id <- beds$id[1]
    md <- bb_metadata(bedbase, id, TRUE)
    expect_equal("bed6+4", md$bed_type)
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

test_that("bb_to_granges returns GRanges object given bed4+2 with genome", {
    beds <- bb_list_beds(bedbase, bed_compliance = "bed4+2")
    id <- beds$id[1]
    md <- bb_metadata(bedbase, id, TRUE)
    expect_equal("bed4+2", md$bed_compliance)
    gro <- bb_to_granges(bedbase, id)
    df <- as.data.frame(gro)
    expect_contains(
        c(
            "seqnames", "start", "end", "width", "strand", "name", "V5", "V6"
        ),
        names(df)
    )
})

test_that("bb_to_granges allows passing extra_cols", {
    beds <- bb_list_beds(bedbase, bed_compliance = "bed6+5")
    id <- beds$id[1]
    md <- bb_metadata(bedbase, id, TRUE)
    expect_equal("bed6+5", md$bed_compliance)
    gro <- bb_to_granges(bedbase, id,
        extra_cols = c(
            "t1" = "character", "t2" = "character", "t3" = "character",
            "t4" = "character", "t5" = "character"
        )
    )
    expect_in(c(
        "seqnames", "start", "end", "width", "strand", "name", "score", "t1",
        "t2", "t3", "t4", "t5"
    ), names(as.data.frame(gro)))
})

test_that("bb_to_grangeslist creates a GRangesList", {
    grl <- bb_to_grangeslist(bedbase, "lola_hg38_ucsc_features")
    expect_true(methods::is((grl)[1], "CompressedGRangesList"))
    expect_equal(11, length(grl))
})

test_that("bb_save saves bed files to a path", {
    path <- tempdir()
    if (!dir.exists(path)) {
        dir.create(path)
    }
    bb_save(bedbase, ex_bed$id, path, quietly = TRUE)
    expect_true(file.exists(file.path(path, paste0(ex_bed$id, ".bed.gz"))))
    bedset <- bb_metadata(bedbase, "lola_hg38_ucsc_features")
    bb_save(bedbase, bedset$id, path, quietly = TRUE)
    for (id in bedset$bed_ids) {
        expect_true(file.exists(file.path(path, paste0(id, ".bed.gz"))))
    }
})
