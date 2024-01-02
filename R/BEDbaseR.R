#' Get metadata
#'
#' @param id BEDbase record identifier or object identifier
#' @param record_type bed, bedset, or objects
#'
#' @importFrom glue glue
#' @importFrom httr2 request req_perform resp_body_json
#'
#' @returns a list with metadata
#'
#' @examples
#' getMetadata("421d2128e183424fcc6a74269bae7934", "bed")
#'
#' @export
getMetadata <- function(id, record_type=c("bed", "bedset", "objects"))
{
    record_type <- match.arg(record_type)
    url <- glue("{BEDBaseBaseUrl}/{record_type}/{id}")
    if (record_type != "objects") {
        url <- glue("{url}/metadata")
    }
    req_perform(request(url)) |> resp_body_json()
}

#' Get BED or BEDsets
#'
#' @param record_type bed or bedset
#'
#' @importFrom glue glue
#' @importFrom httr2 request req_perform resp_body_json
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble as_tibble
#'
#' @returns a tibble of record identifiers and record names
#'
#' @examples
#' recs <- getRecords("bed")
#'
#' @export
getRecords <- function(record_type=c("bed", "bedset"))
{
    record_type <- match.arg(record_type)
    url <- glue("{BEDBaseBaseUrl}/{record_type}/list")
    list_of_records <- req_perform(request(url)) |> resp_body_json()
    tibble_of_records <- tibble()
    if (length(records)) {
        cnames <- names(list_of_records$records[[1]])
        tibble_of_records <- list_of_records$records |>
            map_dfr(function(x) { set_names(unlist(x), cnames) }) |>
                as_tibble()
    }
    tibble_of_records
}

#' Get BED record identifiers associated with BEDset
#'
#' @param record_id record identifier
#'
#' @importFrom tibble as_tibble
#'
#' @returns a list of record identifiers
#'
#' @examples
#' getBedsIn("excluderanges")
#'
#' @export
getBedsIn <- function(record_id)
{
    url <- glue("{BEDBaseBaseUrl}/bedset/{record_id}/bedfiles")
    records <- req_perform(request(url)) |> resp_body_json()
    unlist(records$bedfile_metadata, use.names = FALSE)
}

#' Download BED file
#'
#' @param record_id BEDbase record identifier
#' @param destdir directory to save the file
#' @param access_id Download protocol. "Local" access_identifier is removed.
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
#' grobj <- fileToGRanges(filepath)
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
            hasName <- ifelse(columns >= 4, is.character(df[, 4]), FALSE)
            hasScore <- ifelse(columns >= 5, isScore(df[, 5]) && hasName, FALSE)
            hasStrand <- ifelse(columns >= 6, isStrand(df[, 6]) && hasScore, FALSE)
            hasThickStart <- ifelse(columns >= 7,
                                    is.integer(df[, 7]) && hasStrand,
                                    FALSE)
            hasThickEnd <- ifelse(columns >= 8,
                                  is.integer(df[, 8]) && hasThickStart,
                                  FALSE)
            hasItemRgb <- ifelse(columns >= 9,
                                 isItemRgb(df[, 9]) && hasThickEnd,
                                 FALSE)
            hasBlockCount <- ifelse(columns >= 10,
                                    is.integer(df[, 10]) && hasItemRgb,
                                    FALSE)
            hasBlockSizes <- ifelse(columns >= 11,
                                    matchesBlockCount(df[, 10], df[, 11]) && hasBlockCount,
                                    FALSE)
            hasBlockStarts <- ifelse(columns >= 12,
                                     matchesBlockCount(df[, 10], df[, 12]) && hasBlockSizes,
                                     FALSE)
            if (hasBlockStarts) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4],
                        score=df[, 5], strand=df[, 6],
                        thick=IRanges(df[, 7], df[, 8]), itemRgb=df[, 9],
                        blockCount=df[, 10], blockSizes=df[, 11],
                        blockStarts=df[, 12])
            } else if (hasBlockSizes) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4],
                        score=df[, 5], strand=df[, 6],
                        thick=IRanges(df[, 7], df[, 8]), itemRgb=df[, 9],
                        blockCount=df[, 10], blockSizes=df[, 11])
            } else if (hasBlockCount) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4],
                        score=df[, 5], strand=df[, 6],
                        thick=IRanges(df[, 7], df[, 8]), itemRgb=df[, 9],
                        blockCount=df[, 10])
            } else if (hasItemRgb) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4],
                        score=df[, 5], strand=df[, 6],
                        thick=IRanges(df[, 7], df[, 8]), itemRgb=df[, 9])
            } else if (hasThickEnd) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4],
                        score=df[, 5], strand=df[, 6],
                        thick=IRanges(df[, 7], df[, 8]))
            } else if (hasThickStart) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4],
                        score=df[, 5], strand=df[, 6], thick=IRanges(df[, 7]))
            } else if (hasStrand) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4],
                        score=df[, 5], strand=df[, 6])
            } else if (hasScore) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4],
                        score=df[, 5])
            } else if (hasName) {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]), name=df[, 4])
            } else {
                GRanges(df[, 1], IRanges(df[, 2], df[, 3]))
            }
        })
}

#' Download BED file associated with record identifier and create a GRanges object
#'
#' @param record_id BEDbase record identifier
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
