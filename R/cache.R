#' Get the cache
#'
#' Uses BEDBASE_CACHE for cache path if set; otherwise, it uses the default R
#' cache path for bedbaser.
#'
#' @param quietly logical() (default TRUE) display message
#'
#' @importFrom BiocFileCache BiocFileCache
#' @importFrom rlang inform
#' @importFrom tools R_user_dir
#'
#' @returns BiocFileCache object
#'
#' @examples
#' Sys.setenv("BEDBASE_CACHE" = ".cache/bedbaser")
#' .get_cache()
#'
#' @noRd
.get_cache <- function(quietly = TRUE) {
    path <- ifelse(Sys.getenv("BEDBASE_CACHE") != "",
        Sys.getenv("BEDBASE_CACHE"),
        R_user_dir("bedbaser", which = "cache")
    )
    if (!quietly) {
        inform(paste("Using", path))
    }
    BiocFileCache(path)
}

#' Set the cache
#'
#' Set BEDBASE_CACHE to path
#'
#' @importFrom BiocFileCache BiocFileCache
#' @importFrom rlang inform
#'
#' @returns BiocFileCache object
#'
#' @examples
#' .set_cache(tempdir())
#'
#' @noRd
.set_cache <- function(path, quietly = TRUE) {
    if (is.null(path))
        bfc <- .get_cache(quietly)
    else {
        Sys.setenv("BEDBASE_CACHE" = path)
        if (!quietly) {
            inform(paste("Using", path))
        }
        bfc <-BiocFileCache(path)
    }
    bfc
}

#' Retrieve path from cache or download file and cache
#'
#' This function is described in the BiocFileCache vignette.
#'
#' @param url character() remote resource
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
#' .download_and_cache(url)
#'
#' @noRd
.download_and_cache <- function(url, quietly = TRUE) {
    bfc <- .get_cache()
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
