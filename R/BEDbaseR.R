#' List BED or BEDsets
#'
#' @param record_type bed or bedset
#'
#' @returns a list of bed or bedsets
#'
#' @importFrom glue glue
#' @importFrom httr2 request req_perform resp_body_json
#'
#' @returns a list with metadata
#'
#' @examples
#' recs <- listRecords("bed")
#'
#' @export
listRecords <- function(record_type=c("bed", "bedset"))
{
    record_type <- match.arg(record_type)
    url <- glue("{BEDBaseBaseUrl}/{record_type}/list")
    req_perform(request(url)) |> resp_body_json()
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
#' filepath <-downloadBedFile("ce9f6e8adf5779edb632245a166b92bd", "/tmp")
#' zebrafish <-downloadBedFile("eec93c0649bfc2fc90cdc9fca3f52588", "/tmp")
#' filepath <-downloadBedFile("e6cd0dbb252be66f8c2bdb0d23ed431c", "/tmp")
#' telomere <-downloadBedFile("b56d842d5727f1cfaaccb0bdc1bba6ad", "/tmp")
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

#' Create a GRanges object from a BED file
#'
#' @param filepath path to a BED file
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom rtracklayer import
#' @importFrom IRanges IRanges
#'
#' @returns a GRanges object
#'
#' @examples
#' grobj <- fileToGRanges("421d2128e183424fcc6a74269bae7934")
#'
#' @export
fileToGRanges <- function(filepath)
{
    tryCatch(
        import(filepath, format = "bed")
        ,
        error = function(e) {
          df <- read.table(filepath)
          columns <- length(df)
          stopifnot("File missing required columns" = columns >= 3)
          GRanges(df[, 1], IRanges(df[, 2], df[, 3]))
        })
}


#' Download BED file associated with record_id and create a GRanges object
#'
#' @param record_id BEDbase record id
#'
#' @returns a GRanges object
#'
#' @examples
#' grobj <- importToGRanges("421d2128e183424fcc6a74269bae7934")
#'
#' @export
importToGRanges <- function(record_id)
{
    filepath <- downloadBedFile(record_id, destdir=tempdir())
    fileToGRanges(filepath)
}
