#' Base URL of BEDbase
BEDBASEURL <- "https://api.bedbase.org"

#' Base URL of PEPhub
#PEPHUBURL <- "https://pephub-api.databio.org/api/v1"

#' Construct object identifier
#'
#' Note: only record_type = bed, result_id = bedfile?
#'
#' @param record_id character(1) BEDbase record identifier
#' @param record_type character(1) (default bed) BEDbase record type
#' @param result_id character(1) (default bedfile) BEDbase result identifier
#'
#' @importFrom glue glue
#'
#' @return String
#'
#' @examples
#' obj_id <- make_obj_id("eaf9ee97241f300f1c7e76e1f945141f")
make_obj_id <- function(record_id, record_type="bed", result_id="bedfile")
{
    glue("{record_type}.{record_id}.{result_id}")
}

#' Get valid access identifiers
#'
#' Note: omits 'local' option
#'
#' @param obj_id character(1) BEDbase object record identifier
#'
#' @return character() available access identifiers
#'
#' @examples
#' get_access_ids("bed.421d2128e183424fcc6a74269bae7934.bedfile")
get_access_ids <- function(obj_id) {
    metadata <- get_metadata(obj_id, "object")
    access_methods <- unlist(lapply(metadata$access_methods, `[[`, c('type')))
    remove_index <- match("local", access_methods)
    if (!is.na(remove_index)) {
        access_methods <- access_methods[-remove_index]
    }
    access_methods
}

#' Query BEDbase API
#'
#' Note: The genomes end point error
#'
#' @param endpoint character(1) BEDbase API endpoint
#' @param base_url character(1) (default BEDBASEurl) url
#' @param quiet logical(1) (default FALSE) suppress message
#'
#' @importFrom glue glue
#' @importFrom httr2 req_perform resp_body_json
#'
#' @return a vector or list
#'
#' @examples
#' query_bedbase("bed", "count")
query_bedbase <- function(endpoint, base_url = BEDBASEURL, quiet = FALSE) {
    url <- glue("{baseurl}/{endpoint}")
    if (!quiet)
        message("Requesting", url, "...")
    req_perform(request(url)) |>
        resp_body_json()
}

#' Get service information
#'
#' @importFrom glue glue
#' @importFrom httr2 req_perform resp_body_json
#'
#' @return list() service info, such as version
#'
#' @examples
#' get_service_info()
get_service_info <- function() {
    query_bedbase(glue("{BEDBASEURL}/service-info"))
}
