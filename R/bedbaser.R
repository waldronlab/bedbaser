#' Count BEDs or BEDsets
#'
#' @param rec_type character(1) bed or bedset
#' @param quiet logical(1) (default FALSE) display message
#'
#' @importFrom glue glue
#'
#' @return integer(1) the number of BEDs or BEDsets available
#'
#' @examples
#' count("bed")
#'
#' @export
count <- function(rec_type = c("bed", "bedset"), quiet = FALSE) {
    rec_type <- match.arg(rec_type)
    query_bedbase(glue("{rec_type}/count"))
}

#' Get genome assemblies in BEDbase
#'
#' @param rec_type character(1) BED, BEDset, or object
#' @param quiet logical(1) (default FALSE) display message
#'
#' @importFrom glue glue
#' @importFrom httr2 request req_perform resp_body_json
#'
#' @return list() metadata
#'
#' @examples
#' get_genomes("bed")
#'
#' @export
get_genomes <- function(rec_type = c("bed", "bedset"), quiet = FALSE) {
    rec_type <- match.arg(rec_type)
    query_bedbase(glue("{rec_type}/genomes"), quiet)
}

#' Get metadata for an BED, BEDset, or object
#'
#' @param id integer(1) record or object identifier
#' @param rec_type character(1) BED, BEDset, or object
#' @param quiet logical(1) (default FALSE) display message
#'
#' @importFrom glue glue
#' @importFrom httr2 request req_perform resp_body_json
#'
#' @return list() metadata
#'
#' @examples
#' get_metadata("421d2128e183424fcc6a74269bae7934", "bed")
#'
#' @export
get_metadata <- function(id, rec_type = c("bed", "bedset", "object"),
                         quiet = FALSE) {
    rec_type <- match.arg(rec_type)
    if (rec_type == "object")
        rec_type <- "objects"   # End point for an object record is "objects"
    url <- glue("{rec_type}/{id}")
    if (rec_type %in% c("bed", "bedset"))
        url <- glue("{url}/metadata")
    query_bedbase(url, quiet)
}

#' Get record identifiers and names for BEDs or BEDsets
#'
#' Note: Should be paged
#'
#' @param rec_type character(1) bed or bedset
#' @param quiet logical(1) (default FALSE) display message
#'
#' @importFrom glue glue
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom purrr map_dfr set_names
#' @importFrom tibble tibble as_tibble
#'
#' @return a tibble of record identifiers and record names
#'
#' @examples
#' recs <- get_records("bed")
#'
#' @export
get_records <- function(rec_type = c("bed", "bedset"), quiet = FALSE) {
    records_list <- query_bedbase(glue("{rec_type}/list"), quiet)
    records_tibble <- tibble()
    if (length(records_list)) {
        cnames <- names(records_list$records[[1]])
        records_tibble <- records_list$records |>
            map_dfr(function(x) { set_names(unlist(x), cnames) }) |>
                as_tibble()
    }
    records_tibble
}

#' Get BEDs associated with BEDset
#'
#' @param rec_id integer(1) BEDset record identifier
#' @param quiet logical(1) (default FALSE) display message
#'
#' @importFrom glue glue
#' @importFrom httr2 req_perform request resp_body_json
#' @importFrom tibble as_tibble
#'
#' @return list() BED record identifiers
#'
#' @examples
#' get_beds_in_bedset("excluderanges")
#'
#' @export
get_beds_in_bedset <- function(rec_id, quiet = FALSE) {
    records <- query_bedbase(glue("bedset/{rec_id}/bedfiles"), quiet)
    unlist(records$bedfile_metadata, use.names = FALSE)
}

#' Download BED or thumbnail
#'
#' @param rec_id integer(1) BED record identifier
#' @param file_type character(1) type of file to download, bytes or thumbnail
#' @param acc_id character(1) (default http) access identifier, "local"
#'     is removed.
#' @param quiet logical(1) (default FALSE) display message
#'
#' @importFrom BiocFileCache bfcrpath
#' @importFrom glue glue
#' @importFrom httr2 req_perform request resp_body_json
#' @importFrom stringr str_split_1
#' @importFrom utils download.file tail
#'
#' @return character(1) path to file
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
    url <- glue("objects/{obj_id}/access/{acc_id}")
    if (file_type == "thumbnail")
        url <- glue("{url}/{file_type}")
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
