#' @rdname bedbaser
#' 
#' @title An R client for BEDbase
#' 
#' @description bedbaser exposes the BEDbase API and includes convenience
#' functions for common tasks, including importing a BED id into a `GRanges`
#' object and a BEDset id into a `GRangesList`.
#' 
#' @details
#' 
#' The main functions are as follows
#' * `bedbaser::bedbaser()`: API constructor
#' * `bedbaser::bb_example()`: Retrieve an example BED file or BEDset
#' * `bedbaser::bb_metadata()`: Retrieve metadata for a BED file or BEDset
#' * `bedbaser::bb_list_beds()`: List all BED files
#' * `bedbaser::bb_list_bedsets()`: List all BEDsets
#' * `bedbaser::bb_beds_in_bedset()`: List BED files in BEDset
#' * `bedbaser::bb_bed_text_search()`: Search BED files by text
#' * `bedbaser::bb_to_granges()`: Create a `GRanges` object from a BED id
#' * `bedbaser::bb_to_grangeslist()`: Create a `GrangesList` from a BEDset id
#' Set the environment variable `BEDBASER_CACHE` to alter the cache path for
#' downloaded BED files. 
#' 
#' @importFrom AnVIL Service
#'
#' @returns BEDbase object
#'
#' @examples
#' bedbaser()
#'
#' @export
bedbaser <- function() {
    .BEDbase(
        Service(
            service = "BEDbase",
            host = "api.bedbase.org",
            authenticate = FALSE,
            api_url = "https://api.bedbase.org/openapi.json",
            package = "bedbaser"
        )
    )
}

#' Get the example BED file or BEDset with metadata
#' 
#' @param api API object of BEDbase created from bedbaser()
#' @param rec_type character() bed or bedset
#'
#' @importFrom httr content
#'
#' @returns list() bed or bedset
#'
#' @examples
#' api <- bedbaser()
#' bb_example(api, "bed")
#' bb_example(api, "bedset")
#'
#' @export
bb_example <- function(api, rec_type = c("bed", "bedset")) {
    rec_type <- match.arg(rec_type)
    if (rec_type == "bed")
        rsp <- api$get_example_bed_record_v1_bed_example_get()
    else
        rsp <-api$get_example_bedset_record_v1_bedset_example_get()
    content(rsp)
}

#' Get metadata for a BED file or BEDset
#'
#' @description Get metadata for a BED file or BEDset. Abort
#' if not found or id is not not 32 characters
#'
#' @rdname bb_metadata
#'
#' @param api API object of BEDbase created from bedbaser()
#' @param id integer() record or object identifier
#' @param full logical() (default FALSE) include full record with stats, files,
#' and metadata
#'
#' @importFrom httr content
#' @importFrom rlang abort
#'
#' @returns list() metadata
#'
#' @examples
#' api <- bedbaser()
#'
#' ex_bed <- bb_example(api, "bed")
#' bb_metadata(api, ex_bed$id)
#'
#' ex_bedset <- bb_example(api, "bedset")
#' bb_metadata(api, ex_bedset$id)
#'
#' @export
bb_metadata <- function(api, id, full = FALSE) {
    rsp <- api$get_bed_metadata_v1_bed__bed_id__metadata_get(bed_id = id,
                                                             full = full)
    if (rsp$status_code != 200) {
        rsp <- api$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(
            bedset_id = id,
            full = full)
    }
    result <- content(rsp)
    if (rsp$status_code == 404)
        abort(message = result$detail)
    else if (rsp$status != 200)
        abort(message = "{result$type}: input {result$input} {result$msg}")
    else
        result
}

#' List BEDs
#' 
#' @rdname bb_list_beds
#'
#' @param api API object of BEDbase created from bedbaser()
#' @param genome character() (default NULL) genome keyword
#' @param bed_type character() (default NULL) bed file type
#' @param limit integer() (default 1000) maximum records
#' @param offset integer() (default 0) page token of records
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#'
#' @returns tibble() of BED records
#'
#' @examples
#' api <- bedbaser()
#' bb_list_beds(api)
#'
#' @export
bb_list_beds <- function(api, genome = NULL, bed_type = NULL, limit = 1000,
                         offset = 0) {
    rsp <- api$list_beds_v1_bed_list_get(genome = genome, bed_type = bed_type,
                                         limit = limit, offset = offset)
    recs <- content(rsp)
    results <- tibble()
    if (recs$count)
        results <- map_depth(.x = recs$results, 2, 
                             ~ replace(.x, is.null(.x), NA)) |>
                   bind_rows()
    results
}

#' List BEDsets
#'
#' @rdname bb_list_bedsets
#'
#' @param api API object of BEDbase created from bedbaser()
#' @param query character() (default NULL) keyword
#' @param limit integer() (default 1000) maximum records of bedsets
#' @param offset integer() (default 0) page token of records
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#' @importFrom tidyr unnest
#'
#' @returns tibble() of BEDset records
#'
#' @examples
#' api <- bedbaser()
#' bb_list_bedsets(api)
#'
#' @export
bb_list_bedsets <- function(api, query = NULL, limit = 1000, offset = 0) {
    rsp <- api$list_bedsets_v1_bedset_list_get(query = query,
                                               limit = limit,
                                               offset = offset)
    recs <- content(rsp)
    results <- tibble()
    if (recs$count)
        results <- bind_rows(recs$results) |>
                   unnest(cols = c(bed_ids))
    results
}

#' Get BEDs associated with BEDset
#' 
#' @rdname bb_beds_in_bedset
#'
#' @param api API object of BEDbase created from bedbaser()
#' @param bedset_id integer() BEDset record identifier
#' 
#' @importFrom httr content
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#'
#' @returns list() BED record identifiers
#'
#' @examples
#' api <- bedbaser()
#' ex_bedset <- bb_example(api, "bedset")
#' bb_beds_in_bedset(api, ex_bedset$id)
#'
#' @export
bb_beds_in_bedset <- function(api, bedset_id) {
    rsp <- api$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(
        bedset_id = bedset_id)
    recs <- content(rsp)
    results <- tibble()
    if (recs$count)
        results <- bind_rows(recs$results)
    results
}

#' Search BED files by text
#'
#' Returns all available results scored.
#' 
#' @rdname bb_bed_text_search
#' 
#' @param api API object of BEDbase created from bedbaser()
#' @param query character() keywords to search
#' @param limit integer() (default 10) maximum number of results
#' @param offset integer() (default 0) page offset of results
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#' @importFrom tibble tibble
#' @importFrom utils URLencode
#'
#' @returns tibble()
#'
#' @examples
#' api <- bedbaser()
#' bb_bed_text_search(api, "hg38")
#'
#' @export
bb_bed_text_search <- function(api, query, limit = 10, offset = 0) {
    encoded_query <- URLencode(query, reserved = TRUE)
    rsp <- api$text_to_bed_search_v1_bed_search_text_post(query = encoded_query,
                                                          limit = limit,
                                                          offset = offset)
    recs <- content(rsp)
    results <- tibble()
    if (recs$count) {
        results <- map_depth(.x = recs$results, 1, \(y) unlist(y)) |>
                   bind_rows()
    }
    results
}

#' Create a GRanges object given a BED id
#'
#' @rdname bb_to_granges
#' 
#' @param api API object of BEDbase created from bedbaser()
#' @param bed_id integer() BED record identifier
#' @param file_type character() bed or bigbed
#' @param extra_cols character() (default NULL) extra column names to
#'        construct GRanges objects
#' @param quietly logical() (default TRUE) display messages
#'
#' @importFrom dplyr filter
#' @importFrom R.utils gunzip
#' @importFrom rtracklayer import.bb
#' @importFrom rlang warn
#'
#' @returns GRanges() object
#'
#' @examples
#' api <- bedbaser()
#' ex_bed <- bb_example(api, "bed")
#' bb_to_granges(api, ex_bed$id)
#'
#' @export
bb_to_granges <- function(api, bed_id, file_type = "bed", extra_cols = NULL,
                          quietly = TRUE) {
    stopifnot(file_type %in% c("bed", "bigbed"))
    metadata <- bb_metadata(api, bed_id, TRUE)
    file_path <- .get_file(metadata, file_type, "http", quietly)

    if (file_type == "bed") {
        .bed_file_to_granges(file_path, metadata, extra_cols, quietly)
    } else if (file_type == "bigbed") {
        if (.Platform$OS.type == "windows") {
            warn("This feature does not work on Windows.")
        } else {
            import.bb(file_path, format = "bigBed")
        }
    }
}

#' Create a GRangesList object given a BEDset id
#'
#' @rdname bb_to_grangeslist
#'
#' @param api API object of BEDbase created from bedbaser()
#' @param bedset_id integer() BEDset record identifier
#' @param quietly logical() (default TRUE) display messages
#'
#' @importFrom GenomicRanges GRangesList
#'
#' @returns GRangesList() object
#'
#' @examples
#' api <- bedbaser()
#' id <- "lola_hg38_ucsc_features"
#' bb_to_grangeslist(api, id)
#'
#' @export
bb_to_grangeslist <- function(api, bedset_id, quietly = TRUE) {
    beds <- bb_beds_in_bedset(api, bedset_id)
    gros <- list()
    for (bed_id in beds$id) {
        gro <- bb_to_granges(api, bed_id, quietly = quietly) 
        gros[[length(gros)+1]] <- gro
    }
    GRangesList(gros)
}
