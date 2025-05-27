#' Get file name from URL for a file
#'
#' @param a_url character(1) URL
#'
#' @return character(1) file name
#'
#' @examples
#' url <- "https://this/is/an/example"
#'
#' @noRd
.get_file_name <- function(a_url) {
    url_parts <- unlist(strsplit(a_url, "/"))
    url_parts[length(url_parts)]
}

#' Get BEDbase url for BED file
#'
#' @param records list() metadata
#' @param file_type character(1) bed or bigbed
#' @param access_type character(1) s3 or http
#'
#' @return character(1) url to BED file
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' ex_metadata <- bb_metadata(bedbase, ex_bed$id, TRUE)
#' .get_url(ex_bed$files, "bed", "http")
#'
#' @noRd
.get_url <- function(
        metadata, file_type = c("bed", "bigbed"),
        access_type = c("s3", "http")) {
    file_type <- match.arg(file_type)
    access_type <- match.arg(access_type)
    file_details <- dplyr::bind_rows(metadata$files) |>
        tidyr::unnest_wider(access_methods) |>
        tidyr::unnest_wider(access_url) |>
        dplyr::filter(
            name == paste(file_type, "file", sep = "_"),
            access_id == access_type
        )
    file_details$url
}

#' Get a BED file
#'
#' @description Download or retrieve the file the cache. If not available, get
#' the file from bedbase.org and save to the cache or a path. If a directory
#' does not exist along specified path, it will raise an error message.
#'
#' @param metadata list() full metadata
#' @param cache_or_path [BiocFileCache][BiocFileCache::BiocFileCache-class] or
#' character(1) cache or save path
#' @param file_type character(1) bed or bigbed
#' @param access_type character(1) s3 or http
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return character(1) file path
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' md <- bb_metadata(bedbase, ex_bed$id, TRUE)
#' .get_file(md, tempdir(), "bed", "http")
#'
#' @noRd
.get_file <- function(metadata, cache_or_path, file_type, access_type,
    quietly = TRUE) {
    file_url <- .get_url(metadata, file_type, access_type)
    if (methods::is(cache_or_path, "BiocFileCache")) {
        bed_file <- .cache_bedfile(metadata$id, file_url, cache_or_path)
    } else {
        bed_file <- file.path(cache_or_path, .get_file_name(file_url))
        utils::download.file(file_url, bed_file, quiet = quietly)
    }
    bed_file
}

#' Create a named vector based on the given file
#'
#' @description Create a named vector to pass as `extraCols` to
#' [rtracklayer][rtracklayer::BEDFile-class] based on the file at `file_path`.
#' Subsitute dummy column column names and attempt to match the content of
#' a column to a type.
#'
#' @param file_path character(1) path to BED
#' @param x double(1) the x in BEDX+Y
#' @param y double(1) the y in BEDX+Y
#'
#' @return character(1) representing `extraCols` for
#' [rtracklayer][rtracklayer::BEDFile-class]
#'
#' @examples
#' id <- "608827efc82fcaa4b0bfc65f590ffef8"
#' bedbase <- BEDbase()
#' md <- bb_metadata(bedbase, id, TRUE)
#' file_path <- .get_file(md, getCache(bedbase), "bed", "http")
#' .get_extra_cols(file_path, 3, 9)
#'
#' @noRd
.get_extra_cols <- function(file_path, x, y) {
    t <- utils::read.table(file_path, sep = "\t")
    extra_cols <- c()
    stopifnot(x + y == dim(t)[2])
    t_seq <- seq(from = x + 1, to = x + y)
    for (i in t[t_seq]) {
        if (typeof(i) == "integer") {
            col_type <- "numeric"
        } else {
            col_type <- typeof(i)
        }
        extra_cols <- c(extra_cols, col_type)
    }
    stats::setNames(extra_cols, names(t[t_seq]))
}

#' Import with genome
#'
#' @description Attempt to import a BED file into a
#' [GRanges][GenomicRanges::GRanges-class] object given a genome. If it fails
#' to import with the genome, it is removed to create the GRanges object then
#' the genome is added.
#'
#' @param args list() arguments to create a GRanges object
#'
#' @return GRanges() representing a BED file
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' md <- bb_metadata(bedbase, ex_bed$id, TRUE)
#' file_path <- .get_file(md, getCache(bedbase), "bed", "http")
#' args <- list(
#'     con = file_path,
#'     format = gsub("peak", "Peak", metadata$data_format),
#'     genome = md$genome_alias
#' )
#' .import_with_genome(args)
#'
#' @noRd
.import_with_genome <- function(args) {
    tryCatch(
        do.call(rtracklayer::import, args),
        error = function(e) {
            genome <- args["genome"]
            gro <- do.call(rtracklayer::import, within(args, rm("genome")))
            GenomeInfoDb::genome(gro) <- genome
            gro
        }
    )
}

#' Create GRanges object from a BED file
#'
#' @description Create a [GRanges][GenomicRanges::GRanges-class] object from a
#' BED file. If the BED format is known, `extra_cols` may be used to set the
#' column name and type. For example,
#' \code{extra_cols = c(signalValue = "numeric", pValue = "numeric",
#' qValue = "numeric")}. Aborts if the length of `extra_cols` is not equal to Y
#' in BEDX+Y.
#'
#' @param file_path character(1) path to BED file
#' @param metadata list() full metadata
#' @param extra_cols character() (default \code{NULL}) extra column names to
#' construct a GRanges objects
#' @param quietly logical(1) (default \code{TRUE}) Display information messages
#'
#' @return GRanges() object representing BED
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' md <- bb_metadata(bedbase, ex_bed$id, TRUE)
#' file_path <- .get_file(md, getCache(bedbase), "bed", "http")
#' .bed_file_to_granges(file_path, md)
#'
#' @noRd
.bed_file_to_granges <- function(
        file_path, metadata, extra_cols = NULL,
        quietly = TRUE) {
    args <- list(con = file_path)
    args["format"] <- gsub("peak", "Peak", metadata$data_format)
    nums <- stringr::str_replace(metadata$bed_type, "bed", "") |>
        stringr::str_split_1("\\+") |>
        as.double()

    if (!is.null(extra_cols) && (nums[2] != length(extra_cols))) {
        rlang::abort("`extra_cols` length must match Y value in `bed_type`.")
    }

    if (!grepl("Peak", args["format"]) && nums[2] != 0 && is.null(extra_cols)) {
        if (!quietly) {
            rlang::inform("Assigning column names and types.")
        }
        extra_cols <- .get_extra_cols(file_path, nums[1], nums[2])
    }

    if (!is.null(extra_cols)) {
        args[["extraCols"]] <- extra_cols
    }

    args["genome"] <- metadata$genome_alias

    .import_with_genome(args)
}
