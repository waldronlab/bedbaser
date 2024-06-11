#' BEDbase class
#'
#' @importFrom AnVIL Service
#'
#' @export
.BEDbase <- setClass(
    "BEDbase",
    contains = "Service"
)

#' BEDbase client constructor
#'
#' @return BEDbase object
#'
#' @examples
#' BEDbase()
#'
#' @export
BEDbase <- function() {
    .BEDbase(
        Service(
            "BEDbase",
            host = "api.bedbase.org",
            authenticate = FALSE,
            api_url = "https://api.bedbase.org/openapi.json",
            package = "bedbaser"
        )
    )
}

setGeneric(name = "bb_metadata",
           def = function(x, id, rec_type = c("bed", "bedset"), full = TRUE) {
               standardGeneric("bb_metadata")
})

#' Get metadata for an BED or BEDset
#'
#' @param id integer() record or object identifier
#' @param rec_type character() BED or BEDset
#' @param full logical() include all metadata
#'
#' @importFrom httr content
#'
#' @return list() metadata
#'
#' @examples
#' client <- BEDbase()
#' bb_metadata(client, "0dcdf8986a72a3d85805bbc9493a1302", "bed")
#'
#' @export
setMethod(
    "bb_metadata", "BEDbase",
    function(x, id, rec_type = c("bed", "bedset"), full = TRUE) {
        rec_type <- match.arg(rec_type)
        if (rec_type == "bed")
            rsp <- x$get_bed_metadata_v1_bed__bed_id__metadata_get(bed_id = id,
                                                                   full = full)
        else
            rsp <-
                x$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(bedset_id = id,
                                                                         full = full)
        content(rsp)
    }
)

setGeneric(name = "bb_list_beds",
           def = function(x, genome = NULL, bed_type = NULL, limit = 1000,
                          offset = 0) {
               standardGeneric("bb_list_beds")
})

#' List BEDs
#'
#' @param genome character() (defaults to NULL) genome keyword
#' @param bed_type character() (defaults to NULL) bed file type
#' @param limit integer() (defaults to 1000) maximum records
#' @param offset integer() (defaults to 0) page token of records
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#'
#' @return a tibble of BED records
#'
#' @examples
#' client <- BEDbase()
#' bb_list_beds(client)
#'
#' @export
setMethod(
    "bb_list_beds", "BEDbase",
    function(x, genome = NULL, bed_type = NULL, limit = 1000, offset = 0) {
        rsp <- x$list_beds_v1_bed_list_get(genome = genome, bed_type = bed_type,
                                           limit = limit, offset = offset)
        recs <- content(rsp)
        results <- tibble()
        if (recs$count)
            results <- map_depth(.x = recs$results, 2, ~ replace(.x, is.null(.x), NA)) |>
                       bind_rows()
        results
    }
)

setGeneric(name = "bb_list_bedsets",
           def = function(x, query = NULL, limit = 1000, offset = 0) {
               standardGeneric("bb_list_bedsets")
})

#' List BEDsets
#'
#' @param query character() (defaults to NULL) BEDset keyword
#' @param limit integer() (defaults to 1000) maximum records
#' @param offset integer() (defaults to 0) page token of records
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#'
#' @return a tibble of BEDset records
#'
#' @examples
#' client <- BEDbase()
#' bb_list_bedsets(client)
#'
#' @export
setMethod(
    "bb_list_bedsets", "BEDbase",
    function(x, query = NULL, limit = 1000, offset = 0) {
        rsp <- x$list_bedsets_v1_bedset_list_get(query = query, limit = limit,
                                                 offset = offset)
        recs <- content(rsp)
        results <- tibble()
        if (recs$count)
            results <- bind_rows(recs$results) |>
                       unnest(cols = c(bed_ids))
        results
    }
)

setGeneric(name = "bb_beds_in_bedset",
           def = function(x, rec_id) {
               standardGeneric("bb_beds_in_bedset")
})

#' Get BEDs associated with BEDset
#'
#' @param rec_id integer() BEDset record identifier
#'
#' @importFrom httr content
#'
#' @return list() BED record identifiers
#'
#' @examples
#' client <- BEDbase()
#' bb_beds_in_bedset(client, rec_id = "gse218680")
#' @export
setMethod(
    "bb_beds_in_bedset", "BEDbase",
    function(x, rec_id) {
        rsp <-
            x$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(
                bedset_id = rec_id)
        recs <- content(rsp)
        results <- tibble()
        if (recs$count)
            results <- bind_rows(recs$results)
        results
    }
)

setGeneric(name = "bb_search",
           def = function(x, query, limit = NULL, offset = NULL) {
               standardGeneric("bb_search")
})

#' Search BEDbase
#'
#' @param query character() keywords to search
#' @param limit integer() (defaults to 10) maximum number of results
#' @param offset integer() (defaults to 0) page offset of results
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom utils URLencode
#'
#' @return tibble()
#'
#' @examples
#' client <- BEDbase()
#' bb_search(client, "excluderanges")
#'
#' @export
setMethod(
    "bb_search", "BEDbase",
    function(x, query, limit = 10, offset = 0) {
        encoded_query <- URLencode(query, reserved = TRUE)
        rsp <- x$text_to_bed_search_v1_bed_search_text_post(query = encoded_query,
                                                            limit = limit,
                                                            offset = offset)
        recs <- content(rsp)
        results <- tibble()
        if (recs$count) {
            results <- recs$results
        }
        results
    }
)

setGeneric(name = "bb_metadata_files",
           def = function(x, rec_id) { standardGeneric("bb_metadata_files") })

#' BED files metadata
#'
#' @param rec_id integer() BED record identifier
#'
#' @importFrom httr content
#'
#' @return tibble() records
#'
#' @examples
#' client <- BEDbase()
#' metadata <- bb_metadata_files(client, "bbad85f21962bb8d972444f7f9a3a932")
#'
#' @export
setMethod(
    "bb_metadata_files", "BEDbase",
    function(x, rec_id) {
        rsp <- x$get_bed_files_v1_bed__bed_id__metadata_files_get(rec_id)
        .format_metadata_files(content(rsp))
    }
)

setGeneric(name = "bb_to_granges",
           def = function(x, rec_id, file_type = c("bed", "bigbed"),
                          extra_cols = c(), quietly = FALSE) {
               standardGeneric("bb_to_granges")
})

#' Create a GRanges object given a BED id
#'
#' @param rec_id BEDbase record identifier
#' @param file_type character() bed or bigbed
#' @param extra_cols character() extra column names to construct GRanges objects
#' @param quietly logical() (defaults to FALSE) display messages
#'
#' @importFrom dplyr filter
#' @importFrom R.utils gunzip
#'
#' @return a GRanges object
#'
#' @examples
#' client <- BEDbase()
#' rec_id <- content(client$get_example_bed_record_v1_bed_example_get())$id
#' bb_to_granges(client, rec_id)
#'
#' @export
setMethod(
    "bb_to_granges", "BEDbase",
    function(x, rec_id, file_type = c("bed", "bigbed"), extra_cols = c(),
             quietly = FALSE) {
        file_type <- match.arg(file_type)
        metadata <- bb_metadata(x, rec_id)
        record <- .format_metadata_files(metadata$files) |>
            filter(name == paste(file_type, "file", sep = "_"),
                   access_id == "http")
        gzipfile <- .download_and_cache(record$url, quietly)
        file_path <- gunzip(gzipfile, remove = FALSE)
        .file_to_granges(file_path, metadata$bed_type, metadata$bed_format,
                         extra_cols = extra_cols)
    }
)
