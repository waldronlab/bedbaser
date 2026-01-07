#' Create nested path
#'
#' @description Create directory structure following the BEDbase BED
#' file url or bedset id by creating nested directories in the cache.
#' If the path is for a bedset, the path is given ".txt" as an extension.
#'
#' @param bedbase_path character(1) BED file url or bedset id
#' @param cache [BiocFileCache][BiocFileCache::BiocFileCache-class] bedbaser
#' cache
#'
#' @return character(1) file path
#'
#' @examples
#' bedbase_url <- paste0(
#'     "https://data2.bedbase.org/files/2/6/",
#'     "26a57da7c732a8e63a1dda7ea18af021.bed.gz"
#' )
#' bedbase <- BEDbase(quietly = TRUE)
#' cache <- getCache(bedbase, "bedfiles")
#' .create_nested_path(bedbase_url, cache)
#' # [1] "/path/to/cache/bedfiles/2/6/26a57da7c732a8e63a1dda7ea18af021.bed.gz"
#'
#' @noRd
.create_nested_path <- function(bedbase_path, cache) {
    if (grepl("/", bedbase_path)) {
        file_name <- .get_file_name(bedbase_path)
    } else {
        file_name <- paste0(bedbase_path, ".txt")
    }
    cache_path <- BiocFileCache::bfccache(cache)
    nested_path <- file.path(cache_path, substr(file_name, 1, 1))
    if (!dir.exists(nested_path)) {
        dir.create(nested_path)
    }
    nested_path <- file.path(nested_path, substr(file_name, 2, 2))
    if (!dir.exists(nested_path)) {
        dir.create(nested_path)
    }
    file.path(nested_path, file_name)
}

#' Cache bedset
#'
#' @description Create an entry in the bedsets cache to a nested path for a
#' file named after the BEDset's id that contains the BED ids.
#'
#' @param id character(1) BEDset id
#' @param bedfiles list() BED ids
#' @param cache [BiocFileCache][BiocFileCache::BiocFileCache-class] bedbaser
#' cache
#'
#' @return character(1) local file path
#'
#' @examples
#' bedbase <- BEDbase(quietly = TRUE)
#' id <- "test_bedset"
#' cache <- getCache(bedbase, "bedfiles")
#' beds <- bb_beds_in_bedset(bedbase, id)
#' .cache_bedset_txt(id, beds$id, cache)
#'
#' @noRd
.cache_bedset_txt <- function(id, bedfiles, cache) {
    rpath <- .create_nested_path(id, cache)
    writeLines(bedfiles, rpath)
    rid <- names(BiocFileCache::bfcadd(cache,
        rname = id, fpath = rpath, rtype = "local", rpath = rpath,
        download = FALSE, action = "asis"
    ))
    rpath
}

#' Cache a bed file, downloading if needed, and return a path to the file in
#' the cache.
#'
#' @param id character(1) BEDbase id
#' @param bedbase_url character(1) remote resource
#' @param cache BiocFileCache() object
#'
#' @return character(1) filepath
#'
#' @examples
#' id <- "233479aab145cffe46221475d5af5fae"
#' bedbase_url <- paste0(
#'     "https://data2.bedbase.org/files/2/6/",
#'     "26a57da7c732a8e63a1dda7ea18af021.bed.gz"
#' )
#' .cache_bedfile(id, bedbase_url, BiocFileCache::BiocFileCache(tempdir()))
#'
#' @noRd
.cache_bedfile <- function(id, bedbase_url, cache) {
    rid <- BiocFileCache::bfcquery(cache, id, "rname")$rid
    if (!length(rid)) {
        rpath <- .create_nested_path(bedbase_url, cache)
        curl::curl_download(bedbase_url, rpath)
        rid <- names(BiocFileCache::bfcadd(cache,
            rname = id, fpath = rpath, rtype = "local", rpath = rpath,
            download = FALSE, action = "asis"
        ))
    }
    BiocFileCache::bfcrpath(cache, rids = rid)
}
