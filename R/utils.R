#' Get record type
#'
#' @param id character() BEDbase identifier
#'
#' @importFrom glue glue
#' @importFrom rlang abort
#' @importFrom stringr regex str_detect
#'
#' @return logical()
#'
#' @examples
#' get_rec_type("eaf9ee97241f300f1c7e76e1f945141f")
#'
#' @export
get_rec_type <- function(id) {
    if (str_detect(id, regex("^bed.[:alnum:]+.bedfile$")))
        rec_type <- "bed"
    else if (str_detect(id, regex("^bed.[:alnum:]+.bedset$")))
        rec_type <- "bedset"
    else if (str_detect(id, regex("^[:alnum:]+$")))
        rec_type <- "object"
    else
        abort(glue("Unknown record identifier: {id}"))
    rec_type
}

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

setGeneric(name = "get_bb_service_info",
           def = function(x) { standardGeneric("get_bb_service_info") })

#' Get service information
#'
#' @importFrom httr content
#'
#' @return list() service info, such as version
#'
#' @examples
#' bb <- BEDbase()
#' get_bb_service_info(bb)
#'
#' @export
setMethod(
    "get_bb_service_info", "BEDbase",
    function(x) {
        content(x$service_info_service_info_get())
    }
)
