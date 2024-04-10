client <- BEDbase()

# test that there's an internet connection

test_that("Check bb_metadata for BEDs, BEDsets, and invalid input", {
    ex_bed <- content(client$get_example_bed_record_v1_bed_example_get())
    bed <- bb_metadata(client, ex_bed$record_identifier, "bed")
    expect_identical(ex_bed$metadata, bed)

    ex_bedset <-content(client$get_example_bedset_record_v1_bedset_example_get())
    bedset <- bb_metadata(client, ex_bedset$record_identifier, "bedset")
    expect_identical(ex_bedset$metadata, bedset)
})
