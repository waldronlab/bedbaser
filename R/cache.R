#' Retrieve path from cache or download file and cache
#'
#' This function is described in the BiocFileCache vignette.
#'
#' @param url character() remote resource
#' @param bfc BiocFileCache object
#' @param quietly logical() (default TRUE) display message
#'
#' @importFrom BiocFileCache BiocFileCache bfcadd bfcdownload bfcneedsupdate
#'     bfcquery bfcrpath
#' @importFrom rlang inform
#'
#' @returns filepath character()
#'
#' @examples
#' url <- paste0(
#'     "https://data2.bedbase.org/files/2/6/",
#'     "26a57da7c732a8e63a1dda7ea18af021.bed.gz"
#' )
#' .download_to_cache(url, BiocFileCache::BiocFileCache(tempdir()))
#'
#' @noRd
.download_to_cache <- function(url, bfc, quietly = TRUE) {
    rid <- bfcquery(bfc, url, "rname")$rid
    if (!length(rid)) {
        if (!quietly) {
            inform(paste("Downloading", url, "..."))
        }
        rid <- names(bfcadd(bfc,
            rname = url, fpath = url, rtype = "web",
            download = TRUE, verbose = !quietly
        ))
    }
    bfcrpath(bfc, rids = rid)
}
