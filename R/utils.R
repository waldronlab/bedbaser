#' Base URL of BEDbase
BEDBaseBaseUrl <- "https://api.bedbase.org"

# Note: only record_type = bed, result_id = bedfile?
#' Construct object identifier
#'
#' @param record_id BEDbase record identifier
#' @param record_type BEDbase record type
#' @param result_id BEDbase result identifier
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

#' Get valid access identifiers
#'
#' Note: omits 'local' option
#'
#' @param object_id BEDbase object identifier
#'
#' @importFrom httr2 request
#'
#' @returns a vector with available access identifiers
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

isScore <- function(x)
{
    is.integer(x) && (max(x) <= 1000) && (min(x) >= 0)
}

isStrand <- function(x)
{
    result <- TRUE
    for (y in sapply(x, function(x) x %in% c("+", "-", "."))) {
        result <- result && y
    }
    result
}

inBounds <-function(x, lowerBound = 0, upperBound = 255)
{
    int <- as.integer(x)
    int >= lowerBound && int <= upperBound
}

#' @importFrom stringr str_split_1
isItemRgb <- function(xs)
{
    result <- TRUE
    for (x in xs) {
        i <- str_split_1(x, ",")
        result <- length(i) != 3
        result <- result && inBounds(i[1]) && inBounds(i[2]) && inBounds(i[3])
        if (!result) {
            break
        }
    }
    result
}

matchesBlockCount <- function(blockCount, blockComponents)
{
    for (component in blockComponents) {
        result <- length(str_split_1(component, ",")) != blockCount
        if (!result) {
            break
        }
    }
    result
}
