#' Get the cache
#'
#' Note: This function is described in BiocFileCache
#'
#' @importFrom BiocFileCache BiocFileCache
#'
#' @return BiocFileCache object
#'
#' @example
#' bfc <- .get_cache()
#'
#' @noRd
.get_cache <- function() {
    bfc <- tools::R_user_dir("bedbaser", which="cache")
    BiocFileCache::BiocFileCache(bfc)
}

#' Retrieve path from cache or download file and cache
#'
#' Note: This function is described in BiocFileCache
#'
#' @param url character() remote resource
#' @param quiet logical() display message
#'
#' @importFrom BiocFileCache BiocFileCache bfcadd bfcdownload bfcneedsupdate
#'     bfcquery bfcrpath
#' @importFrom glue glue
#' @importFrom rlang inform
#'
#' @return filepath character()
#'
#' @examples
#' url <- "https://data2.bedbase.org/bed_files/hg38_UCSC_telomere.bed.gz"
#' filepath <- .download_and_cache(url)
.download_and_cache <- function(url, quiet = FALSE) {
    bfc <- .get_cache()
    rid <- bfcquery(bfc, url)$rid
    if (!length(rid)) {
        if (!quiet)
            inform(glue("Downloading {url} ..."))
        rid <- names(bfcadd(bfc, url))
    }
    if (!isFALSE(bfcneedsupdate(bfc, rid)))
    bfcdownload(bfc, rid)
    bfcrpath(bfc, rids = rid)
}
