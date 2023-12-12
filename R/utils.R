#' Base URL of BEDbase
.BEDBaseBaseUrl <- "https://api.bedbase.org"

# Note: only record_type = bed, result_id = bedfile?
#' Construct object identifier
#'
#' @param record_id BEDbase record id
#' @param record_type BEDbase record type
#' @param result_id BEDbase result id
#'
#' @importFrom glue glue
#'
#' @return String
#'
#' @examples
#' object_id <- .getObjectId("eaf9ee97241f300f1c7e76e1f945141f")
.getObjectId <- function(record_id, record_type="bed", result_id="bedfile")
{
    glue("{record_type}.{record_id}.{result_id}")
}

#' Get metadata
#'
#' @param identifier BEDbase record id or object id
#' @param record_type bed, bedset, or objects
#'
#' @importFrom glue glue
#' @importFrom httr2 request req_perform resp_body_json
#'
#' @returns a list with metadata
#'
#' @examples
#' .getMetadata("421d2128e183424fcc6a74269bae7934")
.getMetadata <- function(identifier, record_type=c("bed", "bedset", "objects"))
{
    record_type <- match.arg(record_type)
    url <- glue("{.BEDBaseBaseUrl}/{record_type}/{identifier}")
    if (record_type != "objects") {
        url <- glue("{url}/metadata")
    }
    req_perform(request(url)) |> resp_body_json()
}

#' Get valid access ids
#'
#' Note: omits 'local' option
#'
#' @param record_id BEDbase record id
#'
#' @importFrom httr2 request
#'
#' @returns a vector available access_ids
#'
#' @examples
#' object_id <- .getObjectId("421d2128e183424fcc6a74269bae7934")
#' access_id <- getAccessIds(object_id)
.getAccessIds <- function(object_id)
{
    metadata <- .getMetadata(object_id, "objects")
    access_methods <- unlist(lapply(metadata$access_methods, `[[`, c('type')))
    remove_index <- match("local", access_methods)
    if (!is.na(remove_index)) {
        access_methods <- access_methods[-remove_index]
    }
    access_methods
}

#' Download BED file
#'
#' @param record_id BEDbase record id
#' @param destfile directory and path to save the file
#' @param access_id Download protocol. "Local" access_id is removed.
#'
#' @importFrom glue glue
#' @importFrom stringr str_split_1
#' @importFrom httr2 req_perform resp_body_json
#'
#' @examples
#' filepath <-.downloadBedFile("421d2128e183424fcc6a74269bae7934", "/tmp")
.downloadBedFile <- function(record_id, destfile, access_id="http")
{
    object_id <- .getObjectId(record_id)
    stopifnot(access_id %in% .getAccessIds(object_id))
    url <- glue("{.BEDBaseBaseUrl}/objects/{object_id}/access/{access_id}")
    download_url <- req_perform(request(url)) |> resp_body_json()
    filename <- tail(str_split_1(download_url, "/"), n=1)
    filepath <- glue("{destfile}/{filename}")
    download.file(download_url, filepath)
    filepath
}

#' Download BED file associated with record_id and create a GRanges object
#'
#' @param record_id BEDbase record id
#'
#' @importFrom rtracklayer import
#'
#' @examples
#' grobj <- .importToGRanges("421d2128e183424fcc6a74269bae7934")
.importToGRanges <- function(record_id)
{
    bedfile <- .downloadBedFile(record_id, destfile="/tmp")
    import.bed(bedfile)
}
