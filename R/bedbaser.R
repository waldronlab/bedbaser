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

setGeneric(name = "bb_example",
           def = function(x, rec_type = c("bed", "bedset")) {
               standardGeneric("bb_example")
})

#' Get example BED or BEDset with metadata
#'
#' @param rec_type character() bed or bedset
#'
#' @importFrom httr content
#'
#' @return list() bed or bedset
#'
#' @examples
#' client <- BEDbase()
#' ex_bed <- bb_example(client, "bed")
#' ex_bedset <- bb_example(client, "bedset")
#'
#' @export
setMethod(
    "bb_example", "BEDbase",
    function(x, rec_type = c("bed", "bedset")) {
        rec_type <- match.arg(rec_type)
        if (rec_type == "bed")
            rsp <- x$get_example_bed_record_v1_bed_example_get()
        else
            rsp <-x$get_example_bedset_record_v1_bedset_example_get()
        content(rsp)
    }
)

setGeneric(name = "bb_metadata",
           def = function(x, id, rec_type = c("bed", "bedset"), full = TRUE) {
               standardGeneric("bb_metadata")
})

#' Get metadata for an BED or BEDset
#'
#' Aborts if not found or id is not not 32 characters
#'
#' @param id integer() record or object identifier
#' @param rec_type character() BED or BEDset
#' @param full logical() (default FALSE) include full record with stats, files,
#' and metadata
#'
#' @importFrom httr content
#' @importFrom rlang abort
#'
#' @return list() metadata
#'
#' @examples
#' client <- BEDbase()
#'
#' ex_bed <- bb_example(client, "bed")
#' md <- bb_metadata(client, ex_bed$id, "bed")
#'
#' ex_bedset <- bb_example(client, "bedset")
#' md <- bb_metadata(client, ex_bedset$id, "bedset")
#'
#' @export
setMethod(
    "bb_metadata", "BEDbase",
    function(x, id, rec_type = c("bed", "bedset"), full = FALSE) {
        rec_type <- match.arg(rec_type)
        if (rec_type == "bed")
            rsp <- x$get_bed_metadata_v1_bed__bed_id__metadata_get(
                bed_id = id,
                full = full
            )
        else
            rsp <-
                x$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(
                    bedset_id = id,
                    full = full
                )
        result <- content(rsp)
        if (rsp$status_code == 404)
            abort(message = result$detail)
        else if (rsp$status != 200)
            abort(message = "{result$type}: input {result$input} {result$msg}")
        else
            result
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
#' @return tibble() of BED records
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
#' @param query character() (defaults to NULL) keyword
#' @param limit integer() (defaults to 1000) maximum records of bedsets
#' @param offset integer() (defaults to 0) page token of records
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#' @importFrom tidyr unnest
#'
#' @return tibble() of BEDset records
#'
#' @examples
#' client <- BEDbase()
#' bb_list_bedsets(client)
#'
#' @export
setMethod(
    "bb_list_bedsets", "BEDbase",
    function(x, query = NULL, limit = 1000, offset = 0) {
        rsp <- x$list_bedsets_v1_bedset_list_get(query = query,
                                                 limit = limit,
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
           def = function(x, id) {
               standardGeneric("bb_beds_in_bedset")
})

#' Get BEDs associated with BEDset
#'
#' @param id integer() BEDset record identifier
#'
#' @importFrom httr content
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#'
#' @return list() BED record identifiers
#'
#' @examples
#' client <- BEDbase()
#' ex_bedset <- bb_example(client, "bedset")
#' bb_beds_in_bedset(client, ex_bedset$id)
#'
#' @export
setMethod(
    "bb_beds_in_bedset", "BEDbase",
    function(x, id) {
        rsp <-
            x$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(
                bedset_id = id)
        recs <- content(rsp)
        results <- tibble()
        if (recs$count)
            results <- bind_rows(recs$results)
        results
    }
)

setGeneric(name = "bb_bed_text_search",
           def = function(x, query, limit = NULL, offset = NULL) {
               standardGeneric("bb_bed_text_search")
})

#' Search BED files by text
#'
#' Returns all available results scored.
#'
#' @param query character() keywords to search
#' @param limit integer() (defaults to 10) maximum number of results
#' @param offset integer() (defaults to 0) page offset of results
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#' @importFrom tibble tibble
#' @importFrom utils URLencode
#'
#' @return tibble()
#'
#' @examples
#' client <- BEDbase()
#' bb_bed_text_search(client, "hg38")
#'
#' @export
setMethod(
    "bb_bed_text_search", "BEDbase",
    function(x, query, limit = 10, offset = 0) {
        encoded_query <- URLencode(query, reserved = TRUE)
        rsp <- x$text_to_bed_search_v1_bed_search_text_post(query = encoded_query,
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
)

setGeneric(name = "bb_to_granges",
           def = function(x, id, file_type = c("bed", "bigbed"),
                          extra_cols = NULL, quietly = FALSE) {
               standardGeneric("bb_to_granges")
})

#' Create a GRanges object given a BED id
#'
#' @param id integer() BED record identifier
#' @param file_type character() bed or bigbed
#' @param extra_cols character() (defaults to NULL) extra column names to
#'        construct GRanges objects
#' @param quietly logical() (defaults to FALSE) display messages
#'
#' @importFrom dplyr filter
#' @importFrom R.utils gunzip
#' @importFrom rtracklayer import.bb
#' @importFrom rlang warn
#'
#' @return GRanges() object
#'
#' @examples
#' client <- BEDbase()
#' ex_bed <- bb_example(client, "bed")
#' bb_to_granges(client, ex_bed$id)
#'
#' @export
setMethod(
    "bb_to_granges", "BEDbase",
    function(x, id, file_type = c("bed", "bigbed"), extra_cols = NULL,
             quietly = FALSE) {
        file_type <- match.arg(file_type)
        metadata <- bb_metadata(x, id, "bed", TRUE)
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
)

setGeneric(name = "bb_to_grangeslist",
           def = function(x, id, quietly = FALSE) {
               standardGeneric("bb_to_grangeslist")
})

#' Create a GRangesList object given a BEDset id
#'
#' @param id integer() BEDset record identifier
#' @param quietly logical() (defaults to FALSE) display messages
#'
#' @importFrom GenomicRanges GRangesList
#'
#' @return GRangesList() object
#'
#' @examples
#' client <- BEDbase()
#' id <- "lola_hg38_ucsc_features"
#' bb_to_grangeslist(client, id)
#'
#' @export
setMethod(
    "bb_to_grangeslist", "BEDbase",
    function(x, id, quietly = FALSE) {
        beds <- bb_beds_in_bedset(x, id)
        gros <- list()
        for (id in beds$id) {
            gro <- bb_to_granges(x, id, "bed", quietly = quietly) 
            gros[[length(gros)+1]] <- gro
        }
        GRangesList(gros)
    }
)
