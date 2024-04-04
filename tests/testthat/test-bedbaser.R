client <- BEDbase()

# test that there's an internet connection

test_that("Check bb_count returns integers for BEDs and BEDsets or errors", {
    expect_true(is.integer(bb_count(client, "bed")))
    expect_true(is.integer(bb_count(client, "bedset")))
    expect_error(bb_count(client, "objects"))
})

test_that("Check bb_genomes returns a tibble for BEDs and BEDsets or errors", {
    bed_genomes <- bb_genomes(client, "bed")
    expect_true(is_tibble(bed_genomes))
    expect_contains(bed_genomes$alias, c("ce10", "ce11", "hg19", "hg38"))
    #bedset_genomes <- bb_genomes(client, "bedset")
    #expect_true(is_tibble(bedset_genomes))
    #expect_contains(bedset_genomes$alias, c("ce10", "ce11", "hg19", "hg38"))
    expect_error(bb_genomes(client, "object"))
})

test_that("Check bb_metadata for BEDs, BEDsets, and invalid input", {
    ex_bed <- content(client$get_example_bed_record_bed_example_get())
    bed <- bb_metadata(client, ex_bed$record_identifier, "bed")
    # Removing id
    ex_bed$metadata <- ex_bed$metadata[-38]
    expect_identical(ex_bed$metadata, bed)

    ex_bedset <-content(client$get_example_bedset_record_bedset_example_get())
    bedset <- bb_metadata(client, ex_bedset$record_identifier, "bedset")
    # Removing id
    ex_bedset$metadata <- ex_bedset$metadata[-38]
    expect_identical(ex_bedset$metadata, bedset)
})
