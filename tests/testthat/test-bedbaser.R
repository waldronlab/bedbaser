# test that there's an internet connection

test_that("bb_example has bed_format of 'bed' given rec_type 'bed'", {
    ex_bed <- bb_example(BEDbase(), "bed")
    expect_equal("bed", ex_bed$bed_format)
})

test_that("bb_example has 'bed_ids' given rec_type 'bedset'", {
    ex_bed <- bb_example(BEDbase(), "bedset")
    expect_true("bed_ids" %in% names(ex_bed))
})

test_that("bb_metadata returns metadata for BEDs", {
    api <- BEDbase()
    ex_bed <- bb_example(api, "bed")
    ex_metadata <- content(api$get_bed_metadata_v1_bed__bed_id__metadata_get(ex_bed$id, TRUE))
    bed_metadata <- bb_metadata(api, ex_bed$id, TRUE)
    expect_identical(ex_metadata, bed_metadata)
})

test_that("bb_metadata returns metadata for BEDsets", {
    api <- BEDbase()
    ex_bedset <- bb_example(api, "bedset")
    ex_bedset_metadata <- content(api$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(ex_bedset$id, TRUE))
    bedset_metadata <- bb_metadata(api, ex_bedset$id, TRUE)
    expect_identical(ex_bedset_metadata, bedset_metadata)
})

test_that("bb_metadata errors on invalid input", {
    api <- BEDbase()
    expect_error(bb_metadata(api, "invalid_invalid_invalid_invalid_", "bed"))
})

test_that("bb_list_beds returns same number of results for the hg19 genome", {
    api <- BEDbase()
    hg19_beds_raw <- content(api$list_beds_v1_bed_list_get(genome = "hg19"))
    hg19_bed_names <- unlist(lapply(hg19_beds_raw$results, `[`, c("name")),
        use.names = FALSE
    )
    hg19_beds <- bb_list_beds(api, genome = "hg19")
    expect_equal(hg19_bed_names, hg19_beds$name)
    expect_true(all(hg19_beds$genome_alias == "hg19"))
})

test_that("bb_list_bedsets returns same number for query hg19", {
    api <- BEDbase()
    bedsets_raw <- content(api$list_bedsets_v1_bedset_list_get(query = "hg19"))
    bedsets_names_bed_ids_list <- lapply(bedsets_raw$results, `[`, c("id", "bed_ids"))
    bedsets_names_bed_ids <- bind_rows(bedsets_names_bed_ids_list) |>
        unnest(cols = c(bed_ids))
    bedsets <- bb_list_bedsets(api, query = "hg19")
    expect_equal(bedsets_names_bed_ids$bed_ids, bedsets$bed_ids)
})

test_that("bb_list_bedsets returns number of bed ids", {
    api <- BEDbase()
    bedsets_raw <- content(api$list_bedsets_v1_bedset_list_get(limit = 1))
    bedsets_names_bed_ids_list <- lapply(bedsets_raw$results, `[`, c("id", "bed_ids"))
    bedsets_names_bed_ids <- bind_rows(bedsets_names_bed_ids_list) |>
        unnest(cols = c(bed_ids))
    bedsets <- bb_list_bedsets(api, limit = 1)
    expect_equal(bedsets$bed_ids, bedsets_names_bed_ids$bed_ids)
})

test_that("bb_beds_in_bedset returns expected bed_ids", {
    api <- BEDbase()
    ex_bedset <- content(api$get_example_bedset_record_v1_bedset_example_get())
    ex_bedset_raw <- content(api$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(ex_bedset$id))
    ex_bed_ids_list <- lapply(ex_bedset_raw$results, `[`, c("id"))
    ex_bed_ids <- unlist(ex_bed_ids_list, use.names = FALSE)
    bed_ids <- bb_beds_in_bedset(api, ex_bedset$id)$id
    expect_equal(ex_bed_ids, bed_ids)
})

test_that("bb_bed_text_search returns results scored against the query", {
    api <- BEDbase()
    beds <- bb_bed_text_search(api, "hg38")
    ex_beds <- content(api$text_to_bed_search_v1_bed_search_text_post(
        query = "hg38",
        limit = 10,
        offset = 0
    ))
    ex_beds <- map_depth(.x = ex_beds$results, 1, \(y) unlist(y)) |>
        bind_rows()
    expect_equal(ex_beds, beds)
    expect_true("score" %in% names(beds))
})

test_that("bb_to_granges returns a GRanges object given a 3+0 bed file", {
    api <- BEDbase()
    id <- "b7cb28278e4cba2dc62e51ddc70cfbfb"
    md <- bb_metadata(api, id, TRUE)
    expect_equal("bed3+0", md$bed_type)
    gro <- bb_to_granges(api, id)
    expect_equal("GRanges", class(gro)[1])
})

test_that("bb_to_granges returns a GRanges object given a bigBed file", {
    api <- BEDbase()
    ex_bed <- bb_example(api, "bed")
    if (.Platform$OS.type != "windows") {
        gro <- bb_to_granges(api, ex_bed$id, "bigbed")
        expect_equal("GRanges", class(gro)[1])
    } else {
        expect_warning(rlang::warn("This feature does not work on Windows."),
                       bb_to_granges(api, ex_bed$id, "bigbed"))
    }
})

test_that("bb_to_granges returns a GRanges object given narrowpeak (6+4) file", {
    api <- BEDbase()
    id <- "bbad85f21962bb8d972444f7f9a3a932"
    md <- bb_metadata(api, id, TRUE)
    expect_equal("bed6+4", md$bed_type)
    gro <- bb_to_granges(api, id)
    expect_equal("GRanges", class(gro)[1])
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
    api <- BEDbase()
    id <- "608827efc82fcaa4b0bfc65f590ffef8"
    md <- bb_metadata(api, id, TRUE)
    expect_equal("bed3+9", md$bed_type)
    gro <- bb_to_granges(api, id)
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
    api <- BEDbase()
    ex_bed <- bb_example(api, "bed")
    md <- bb_metadata(api, ex_bed$id, TRUE)
    expect_equal("bed4+1", md$bed_type)
    gro <- bb_to_granges(api, ex_bed$id,
        extra_cols = c("testing" = "character")
    )
    df <- as.data.frame(gro)
    expect_contains(c(
        "seqnames", "start", "end", "width", "strand", "name",
        "testing"
    ), names(df))
})

test_that("bb_to_grangeslist creates a GRangesList", {
    api <- BEDbase()
    grl <- bb_to_grangeslist(api, "lola_hg38_ucsc_features")
    expect_equal("CompressedGRangesList", class(grl)[1])
    expect_equal(10, length(grl))
})
