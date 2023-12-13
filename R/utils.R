#' Base URL of BEDbase
BEDBaseBaseUrl <- "https://api.bedbase.org"

# Note: only record_type = bed, result_id = bedfile?
#' Construct object identifier
#'
#' @param record_id BEDbase record id
#' @param record_type BEDbase record type
#' @param result_id BEDbase result id
#'
#' @importFrom glue glue
#'
#' @returns String
#'
#' @examples
#' object_id <- getObjectId("eaf9ee97241f300f1c7e76e1f945141f")
getObjectId <- function(record_id, record_type="bed", result_id="bedfile")
{
    glue("{record_type}.{record_id}.{result_id}")
}

#' Get valid access ids
#'
#' Note: omits 'local' option
#'
#' @param object_id BEDbase object id
#'
#' @importFrom httr2 request
#'
#' @returns a vector with available access_ids
#'
#' @examples
#' getAccessIds("bed.421d2128e183424fcc6a74269bae7934.bedfile")
getAccessIds <- function(object_id)
{
    metadata <- getMetadata(object_id, "objects")
    access_methods <- unlist(lapply(metadata$access_methods, `[[`, c('type')))
    remove_index <- match("local", access_methods)
    if (!is.na(remove_index)) {
        access_methods <- access_methods[-remove_index]
    }
    access_methods
}
