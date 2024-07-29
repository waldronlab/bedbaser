#' Get the cache
#'
#' Uses BEDBASER_CACHE for cache path if set; otherwise, it uses the default R
#' cache path for bedbaser.
#'
#' @param quietly logical() display message
#'
#' @importFrom BiocFileCache BiocFileCache bfccache
#' @importFrom tools R_user_dir
#'
#' @return BiocFileCache object
#'
#' @examples
#' Sys.setenv("BEDBASER_CACHE"=".cache/bedbaser")
#' bfc <- .get_cache()
#' @noRd
.get_cache <- function(quietly = FALSE) {
    bfc <- ifelse(Sys.getenv("BEDBASER_CACHE") != "",
                  Sys.getenv("BEDBASER_CACHE"),
                  R_user_dir("bedbaser", which="cache"))
    if (!quietly) {
        print(paste("Using", bfccache()))
    }
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
