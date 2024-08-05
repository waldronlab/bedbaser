#' Format BED file metadata
#'
#' @param records list() metadata
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unnest_wider
#'
#' @returns tibble() file metadata
#'
#' @examples
#' api <- bedbaser()
#' ex_bed <- bb_example(api, "bed")
#' ex_metadata <- bb_metadata(api, ex_bed$id, TRUE)
#' .format_metadata_files(ex_bed$files)
#'
#' @noRd
.format_metadata_files <- function(metadata) {
    bind_rows(metadata) |>
        unnest_wider(access_methods) |>
        unnest_wider(access_url)
}

#' Get a file from BEDbase
#'
#' @param metadata list() full metadata
#' @param file_type character() bed or bigbed
#' @param access_type character() s3 or http
#' @param quietly logical() (default TRUE) display messages
#'
#' @importFrom R.utils gunzip
#' @importFrom dplyr filter
#'
#' @returns character() file path
#'
#' @examples
#' api <- bedbaser()
#' ex_bed <- bb_example(api, "bed")
#' md <- bb_metadata(api, ex_bed$id, TRUE)
#' .get_file(md, "bed", "http")
#'
#' @noRd
.get_file <- function(metadata, file_type = c("bed", "bigbed"),
    access_type = c("s3", "http"), quietly = TRUE) {
    file_details <- .format_metadata_files(metadata$files) |>
        filter(
            name == paste(file_type, "file", sep = "_"),
            access_id == access_type
        )
    gzipfile <- .download_and_cache(file_details$url, quietly)
    tryCatch(
        gunzip(gzipfile, remove = FALSE),
        error = function(e) {
            gsub(".gz", "", gzipfile)
        }
    )
}

#' Get extra_cols
#'
#' @param file_path character() path to BED
#' @param x double() the x in BEDX+Y
#' @param y double() the y in BEDX+Y
#'
#' @importFrom stats setNames
#' @importFrom utils read.table
#'
#' @returns vector representing extraCols for rtracklayer
#'
#' @examples
#' id <- "608827efc82fcaa4b0bfc65f590ffef8"
#' md <- bb_metadata(api, id, TRUE)
#' file_path <- .get_file_path(
#'     md$files$bed_file$access_methods[[1]]$access_url$url,
#'     "bed"
#' )
#' .get_extra_cols(file_path, 3, 9)
#'
#' @noRd
.get_extra_cols <- function(file_path, x, y) {
    t <- read.table(file_path, sep = "\t")
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
    setNames(extra_cols, names(t[t_seq]))
}

#' Create GRanges object from a BED file
#'
#' If the BED format is known, `extra_cols` may be used to set the column name
#' and type. For example, `extra_cols = c(signalValue = "numeric",
#' pValue = "numeric", qValue = "numeric")`.
#'
#' Aborts if the length of `extra_cols` is not equal to Y in BEDX+Y.
#'
#' @param file_path character() path to BED file
#' @param metadata list() full metadata
#' @param extra_cols character() (default NULL) extra column names to construct
#'     GRanges objects
#' @param quietly boolean() (default TRUE) Display information messages
#'
#' @importFrom rlang abort inform
#' @importFrom rtracklayer import
#' @importFrom stringr str_replace str_split_1
#'
#' @returns GRanges() object representing BED
#'
#' @examples
#' api <- bedbaser()
#' ex_bed <- bb_example(api, "bed")
#' md <- bb_metadata(api, ex_bed$id, TRUE)
#' file_path <- .get_file(md, "bed", "http")
#' .bed_file_to_granges(file_path, md)
#'
#' @noRd
.bed_file_to_granges <- function(file_path, metadata, extra_cols = NULL,
    quietly = TRUE) {
    bed_format <- gsub("peak", "Peak", metadata$bed_format)
    nums <- str_replace(metadata$bed_type, "bed", "") |>
        str_split_1("\\+") |>
        as.double()

    if (!is.null(extra_cols) && (nums[2] != length(extra_cols))) {
        abort("`extra_cols` length must match the Y value in `bed_type`.")
    }

    if (is.null(extra_cols) && !(grepl("Peak", bed_format)) && nums[2] != 0) {
        extra_cols <- .get_extra_cols(file_path, nums[1], nums[2])
    }

    if (!quietly) {
        inform(paste("Detected", bed_format, "BED file."))
        if (bed_format == "nonstandard") {
            inform("Assigning column names and types.")
        }
    }

    if (grepl("Peak", bed_format) || nums[2] == 0) {
        import(file_path, format = bed_format, genome = metadata$genome_alias)
    } else {
        import(file_path,
            format = "bed", extraCols = extra_cols,
            genome = metadata$genome_alias
        )
    }
}
