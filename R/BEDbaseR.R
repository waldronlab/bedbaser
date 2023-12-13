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
#' getMetadata("421d2128e183424fcc6a74269bae7934")
#'
#' @export
getMetadata <- function(identifier, record_type=c("bed", "bedset", "objects"))
{
    record_type <- match.arg(record_type)
    url <- glue("{BEDBaseBaseUrl}/{record_type}/{identifier}")
    if (record_type != "objects") {
        url <- glue("{url}/metadata")
    }
    req_perform(request(url)) |> resp_body_json()
}

#' Download BED file
#'
#' @param record_id BEDbase record id
#' @param destdir directory to save the file
#' @param access_id Download protocol. "Local" access_id is removed.
#'
#' @importFrom glue glue
#' @importFrom httr2 req_perform request resp_body_json
#' @importFrom stringr str_split_1
#' @importFrom utils download.file tail
#'
#' @returns file path
#'
#' @examples
#' filepath <-downloadBedFile("421d2128e183424fcc6a74269bae7934", "/tmp")
#'
#' @export
downloadBedFile <- function(record_id, destdir, access_id="http")
{
    object_id <- getObjectId(record_id)
    stopifnot(access_id %in% getAccessIds(object_id))
    url <- glue("{BEDBaseBaseUrl}/objects/{object_id}/access/{access_id}")
    download_url <- req_perform(request(url)) |> resp_body_json()
    filename <- tail(str_split_1(download_url, "/"), n=1)
    filepath <- file.path(destdir, filename)
    download.file(download_url, filepath)
    filepath
}

#' Download BED file associated with record_id and create a GRanges object
#'
#' @param record_id BEDbase record id
#'
#' @importFrom rtracklayer import.bed
#'
#' @returns a GRanges object
#'
#' @examples
#' grobj <- importToGRanges("421d2128e183424fcc6a74269bae7934")
#'
#' @export
importToGRanges <- function(record_id)
{
    bedfile <- downloadBedFile(record_id, destdir=tempdir())
    import.bed(bedfile)
}
