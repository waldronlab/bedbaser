client <- BEDbase()

# test that there's an internet connection

test_that("bb_metadata returns metadata for BEDs, BEDsets but errors on invalid input", {
    ex_bed <- content(client$get_example_bed_record_v1_bed_example_get())
    bed_metadata <- bb_metadata(client, ex_bed$id, "bed")
    expect_identical(ex_bed, bed_metadata)

    ex_bedset <-content(client$get_example_bedset_record_v1_bedset_example_get())
    bedset <- bb_metadata(client, ex_bedset$id, "bedset")
    expect_identical(ex_bedset, bedset)

    badbed <- bb_metadata(client, "invalididentifier", "bed")
    expect_identical(badbed$detail[[1]]$msg,
                     "String should have at least 32 characters")
})

test_that("bb_list_beds for hg19 returns a list", {
    hg19_beds <- bb_list_beds(client, genome = "hg19")
    expect_gt(length(hg19_beds), 0)
})


