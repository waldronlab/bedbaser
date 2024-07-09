#' Get the cache
#'
#' This function is described in the BiocFileCache vignette.
#'
#' @importFrom BiocFileCache BiocFileCache
#' @importFrom tools R_user_dir
#'
#' @return BiocFileCache object
#'
#' @example
#' bfc <- .get_cache()
#'
#' @noRd
.get_cache <- function() {
    bfc <- R_user_dir("bedbaser", which="cache")
    BiocFileCache(bfc)
}

#' Retrieve path from cache or download file and cache
#'
#' This function is described in the BiocFileCache vignette.
#'
#' @param url character() remote resource
#' @param quietly logical() display message
#'
#' @importFrom BiocFileCache BiocFileCache bfcadd bfcdownload bfcneedsupdate
#'     bfcquery bfcrpath
#' @importFrom rlang inform
#'
#' @return filepath character()
#'
#' @examples
#' url <- "https://data2.bedbase.org/bed_files/hg38_UCSC_telomere.bed.gz"
#' filepath <- .download_and_cache(url)
.download_and_cache <- function(url, quietly = FALSE) {
    bfc <- .get_cache()
    rid <- bfcquery(bfc, url)$rid
    if (!length(rid)) {
        if (!quietly)
            inform(paste("Downloading", url, "..."))
        rid <- names(bfcadd(bfc, url))
    }
    if (!isFALSE(bfcneedsupdate(bfc, rid)))
    bfcdownload(bfc, rid, ask = FALSE)
    bfcrpath(bfc, rids = rid)
}
