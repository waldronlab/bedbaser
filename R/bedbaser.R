#' Count BEDs or BEDsets
#'
#' @param rec_type character(1) bed or bedset
#'
#' @return integer(1) the number of BEDs or BEDsets available
#'
#' @examples
#' count("bed")
#'
#' @export
count <- function(rec_type = c("bed", "bedset")) {
    rec_type <- match.arg(rec_type)
    query_bedbase(rec_type, "count")
}

#' Get metadata for an BED, BEDset, or object
#'
#' @param id integer(1) record or object identifier
#' @param rec_type character(1) BED, BEDset, or object
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
get_metadata <- function(id, rec_type = c("bed", "bedset", "object")) {
    rec_type <- match.arg(rec_type)
    url <- glue("{rec_type}/{id}")
    if (rec_type != "object")
        url <- glue("{url}/metadata")
    query_bedbase(url)
}

#' Get record identifiers and names for BEDs or BEDsets
#'
#' Note: Should be paged
#'
#' @param rec_type character(1) bed or bedset
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
get_records <- function(rec_type = c("bed", "bedset")) {
    records_list <- query_bedbase(glue("{rec_type}/list"))
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
get_beds_in_bedset <- function(rec_id) {
    url <- glue("bedset/{rec_id}/bedfiles")
    records <- query_bedbase(url)
    unlist(records$bedfile_metadata, use.names = FALSE)
}

#' Download BED
#'
#' @param rec_id integer(1) BED record identifier
#' @param destdir character(1) directory to save the file
#' @param file_type character(1) type of file to download, bytes or thumbnail
#' @param acc_id character(1) (default http) access identifier, "local"
#'     is removed.
#' @param quiet logical(1) (default TRUE) if FALSE, prints message
#'
#' @importFrom glue glue
#' @importFrom httr2 req_perform request resp_body_json
#' @importFrom stringr str_split_1
#' @importFrom utils download.file tail
#'
#' @return file path
#'
#' @examples
#' filepath <-download_bed_file("421d2128e183424fcc6a74269bae7934", "/tmp", "bytes")
#' filepath <-download_bed_file("ce9f6e8adf5779edb632245a166b92bd", "/tmp", "bytes")
#' zebrafish <-download_bed_file("eec93c0649bfc2fc90cdc9fca3f52588", "/tmp", "bytes")
#' filepath <-download_bed_file("e6cd0dbb252be66f8c2bdb0d23ed431c", "/tmp", "bytes")
#' telomere <-download_bed_file("b56d842d5727f1cfaaccb0bdc1bba6ad", "/tmp", "bytes")
#'
#' @export
download_bed_file <- function(rec_id,
                              destdir,
                              file_type = c("bytes", "thumbnail"),
                              acc_id = "http",
                              quiet = TRUE) {
    obj_id <- make_obj_id(rec_id)
    stopifnot(access_id %in% get_access_ids(obj_id))
    url <- glue("objects/{obj_id}/access/{acc_id}/{file_type}")
    download_url <- query_bedbase(url) 
    filename <- tail(str_split_1(download_url, "/"), n=1)
    filepath <- file.path(destdir, filename)
    download.file(download_url, filepath, quiet = quiet)
    filepath
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
