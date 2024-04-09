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

setGeneric(name = "bb_count",
           def = function(x, rec_type = c("bed", "bedset")) {
               standardGeneric("bb_count")
})

#' Count BEDs or BEDsets
#'
#' @param rec_type character() bed or bedset
#'
#' @importFrom httr content
#'
#' @return integer() the number of BEDs or BEDsets available
#'
#' @examples
#' client <- BEDbase()
#' bb_count(client, "bed")
#'
#' @export
setMethod(
    "bb_count", "BEDbase",
    function(x, rec_type = c("bed", "bedset")) {
        rec_type <- match.arg(rec_type)
        if (rec_type == "bed")
            content(x$count_bed_record_bed_count_get())
        else
            content(x$get_bedset_count_bedset_count_get())
    }
)

setGeneric(name = "bb_genomes",
           def = function(x, rec_type = c("bed", "bedset")) {
               standardGeneric("bb_genomes")
})

#' Get genome assemblies in BEDbase
#'
#' @param rec_type character() BED or BEDset
#'
#' @importFrom httr content
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#'
#' @return tibble
#'
#' @examples
#' client <- BEDbase()
#' bb_genomes(client, "bed")
#'
#' @export
setMethod(
    "bb_genomes", "BEDbase",
    function(x, rec_type = c("bed", "bedset")) {
        rec_type <- match.arg(rec_type)
        if (rec_type == "bed")
            resp <- x$get_bed_genome_assemblies_bed_genomes_get()
        else
            resp <- x$get_bedset_genome_assemblies_bedset_genomes_get()
        genome_list <- content(resp)
        genome_tibble <- tibble()
        if (length(genome_list)) {
            cnames <- names(genome_list[[1]])
            genome_tibble <- genome_list |>
                map_dfr(function(y) { set_names(unlist(y), cnames) })
        }
        genome_tibble
    }
)

setGeneric(name = "bb_metadata",
           def = function(x, id, rec_type = c("bed", "bedset")) {
               standardGeneric("bb_metadata")
})

#' Get metadata for an BED or BEDset
#'
#' @param id integer() record or object identifier
#' @param rec_type character() BED or BEDset
#'
#' @importFrom httr content
#'
#' @return list() metadata
#'
#' @examples
#' client <- BEDbase()
#' bb_metadata(client, "421d2128e183424fcc6a74269bae7934", "bed")
#'
#' @export
setMethod(
    "bb_metadata", "BEDbase",
    function(x, id, rec_type = c("bed", "bedset")) {
        rec_type <- match.arg(rec_type)
        if (rec_type == "bed")
            resp <- x$get_bed_metadata_bed__bed_id__metadata_get(id)
        else
            resp <- x$get_bedset_metadata_bedset__bedset_id__metadata_get(id)
        record <- content(resp)
        record$metadata
    }
)

setGeneric(name = "bb_records",
           def = function(x, rec_type, limit = NULL, token = NULL) {
               standardGeneric("bb_records")
})

#' Get record identifiers and names for BEDs or BEDsets
#'
#' Note: how to get next page token
#'
#' @param rec_type character() bed or bedset
#' @param limit integer() (defaults to NULL) maximum records
#' @param token integer() (defaults to NULL) page token of records
#'
#' @importFrom httr content
#' @importFrom purrr map_dfr set_names
#' @importFrom tibble tibble as_tibble
#'
#' @return a tibble of record identifiers and record names
#'
#' @examples
#' client <- BEDbase()
#' bb_records(client, "bed")
#'
#' @export
setMethod(
    "bb_records", "BEDbase",
    function(x, rec_type = c("bed", "bedset"), limit = NULL, token = NULL) {
        rec_type <- match.arg(rec_type)
        if (rec_type == "bed")
            resp <- x$list_beds_bed_list_get(limit=limit, token=token)
        else
            resp <- x$list_bedsets_bedset_list_get(limit=limit, token=token)
        records_tibble <- tibble()
        records_list <- content(resp)
        if (length(records_list)) {
            cnames <- names(records_list$records[[1]])
            records_tibble <- records_list$records |>
                map_dfr(function(x) { set_names(unlist(x), cnames) }) |>
                    as_tibble()
        }
        records_tibble
    }
)

setGeneric(name = "bb_beds_in_bedset",
           def = function(x, rec_id) { standardGeneric("bb_beds_in_bedset") })

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
#' bb_beds_in_bedset(client, "bed")
#' rec_id <- "421d2128e183424fcc6a74269bae7934"
#' @export
setMethod(
    "bb_beds_in_bedset", "BEDbase",
    function(x, rec_id) {
        response <-
            x$get_bedfiles_in_bedset_bedset__bedset_id__bedfiles_get(rec_id)
        records <- content(resp)
        unlist(records$bedfile_metadata, use.names = FALSE)
    }
)

setGeneric(name = "bb_search",
           def = function(x, query, limit = NULL, offset = NULL) {
               standardGeneric("bb_search")
})

#' Search BEDbase
#'
#' @param query character() keywords to search
#' @param limit integer() (defaults to NULL) maximum number of results
#' @param offset integer() (defaults to NULL) page offset of results
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom tibble as_tibble
#' @importFrom utils URLencode
#'
#' @return tibble()
#'
#' @examples
#' client <- BEDbase()
#' bb_search(client, "excluderanges")
#'
#' @export
bb_search <- function(x, query, limit = NULL, offset = NULL) {
    encoded_query <- URLencode(query, reserved = TRUE)
    resp <- x$text_to_bed_search_search_bed__query__get(encoded_query,
                                                        limit = limit,
                                                        offset = offset)
    results <- content(resp)
    if (!length(results))
        return(results)
    metadata <- tibble()
    for (result in results) {
        fmd <- list()
        for (i in 1:length(result$metadata)) {
            if (length(result$metadata[[i]]) > 1) {
                cname <- names(result$metadata[i])
                cnames <- sapply(names(result$metadata[[i]]), function(y) {
                    paste(cname, y, sep="_")
                })
                names(result$metadata[[i]]) <- c(cnames)
                submetadata <- unlist(result$metadata[[i]], recursive = TRUE)
                fmd <- append(fmd, submetadata)
            } else {
                fmd <- append(fmd, result$metadata[i])
            }
        }
        bind_rows(metadata, as_tibble(fmd))
    }
    metadata
}

#' Download BED or thumbnail
#'
#' @param rec_id integer() BED record identifier
#' @param file_type character() type of file to download, bytes or thumbnail
#' @param acc_id character() (default http) access identifier, "local"
#'     is removed.
#' @param quiet logical() (default FALSE) display message
#'
#' @importFrom BiocFileCache bfcrpath
#' @importFrom httr2 req_perform request resp_body_json
#' @importFrom stringr str_split_1
#' @importFrom utils download.file tail
#'
#' @return character() path to file
#'
#' @examples
#' telomere <- download_file("80d2b2581bb25fa6b73ec56c11969fb3", "bytes")
#'
#' @export
download_file <- function(rec_id, file_type = c("bytes", "thumbnail"),
                          acc_id = "http", quiet = FALSE) {
    obj_id <- make_obj_id(rec_id)
    stopifnot(acc_id %in% get_access_ids(obj_id, TRUE))
    file_type <- match.arg(file_type)
    url <- paste0("objects/", obj_id, "/access/", acc_id)
    if (file_type == "thumbnail")
        url <- paste(url, file_type, sep="/")
    download_url <- query_bedbase(url, quiet)
    .download_and_cache(download_url, quiet)
}

##' Create a GRanges object from a BED file
##'
##' @param filepath path to a BED file
##'
##' @importFrom GenomicRanges GRanges
##' @importFrom rtracklayer import
##' @importFrom IRanges IRanges
##'
##' @return a GRanges object
##'
##' @examples
##' grobj <- file_to_granges(filepath)
##'
##' @export
#file_to_granges <- function(filepath) {
#}

##' Download BED file by record identifier and create a GRanges object
##'
##' @param rec_id BEDbase record identifier
##'
##' @return a GRanges object
##'
##' @examples
##' grobj <- to_granges("421d2128e183424fcc6a74269bae7934")
##'
##' @export
#to_granges <- function(rec_id) {
#    filepath <- download_bed_file(rec_id, destdir=tempdir())
#    file_to_granges(filepath)
#}
