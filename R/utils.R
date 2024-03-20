#' Base URL of BEDbase
BEDBASEURL <- "https://api.bedbase.org"

#' Construct object identifier
#'
#' Note: only rec_type = bed, result_id = bedfile?
#'
#' @param rec_id character() BEDbase record identifier
#' @param rec_type character() (default bed) BEDbase record type
#' @param result_id character() (default bedfile) BEDbase result identifier
#'
#' @importFrom glue glue
#'
#' @return String
#'
#' @examples
#' obj_id <- make_obj_id("eaf9ee97241f300f1c7e76e1f945141f")
make_obj_id <- function(rec_id, rec_type = "bed", result_id = "bedfile")
{
    glue("{rec_type}.{rec_id}.{result_id}")
}

#' Get valid access identifiers
#'
#' Note: omits 'local' option
#'
#' @param obj_id character() BEDbase object record identifier
#' @param quiet logical() (default FALSE) display message
#'
#' @return character() available access identifiers
#'
#' @examples
#' get_access_ids("bed.421d2128e183424fcc6a74269bae7934.bedfile")
get_access_ids <- function(obj_id, quiet = FALSE) {
    metadata <- get_metadata(obj_id, "object", quiet = FALSE)
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
#' @param endpoint character() BEDbase API endpoint
#' @param quiet logical() (default FALSE) display message
#'
#' @importFrom glue glue
#' @importFrom httr2 req_perform resp_body_json
#' @importFrom rlang inform abort
#'
#' @return a vector or list
#'
#' @examples
#' query_bedbase("bed/count")
query_bedbase <- function(endpoint, quiet = FALSE) {
    url <- glue("{BEDBASEURL}/{endpoint}")
    if (!quiet)
        inform(glue("Requesting {url} ..."))
    tryCatch(
        error =  function(cnd) {
            abort("offline", message = "Can't access https://api.bedbase.org")
        },
        {
            req_perform(request(url)) |>
                resp_body_json()
        }
    )
}

#' Get service information
#'
#' @param quiet logical() (default FALSE) display message
#'
#' @return list() service info, such as version
#'
#' @examples
#' get_service_info()
get_service_info <- function(quiet = FALSE) {
    query_bedbase("service-info", quiet)
}
