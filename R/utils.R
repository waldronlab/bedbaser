#' Format BED file metadata
#'
#' @param records list() metadata
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map_depth
#' @importFrom tidyr unnest_wider
#'
#' @return tibble() file metadata
#'
#' @examples
#' client <- BEDbase()
#' ex_bed <- bb_example(client, "bed")
#' ex_metadata <-bb_metadata(client, ex_bed$id, "bed", TRUE)
#' .format_metadata_files(ex_bed$files)
.format_metadata_files <- function(metadata) {
    bind_rows(metadata) |>
        unnest_wider(access_methods) |>
        unnest_wider(access_url)
}

#' Get a file from BEDbase
#'
#' @param file_url character() url to file
#' @param file_type character() bed or bigbed
#' @param quietly logical() (defaults to FALSE) display messages
#'
#' @importFrom R.utils gunzip
#'
#' @return character() file path
#'
#' @examples
#' client <- BEDbase()
#' ex_bed <- bb_example(client, "bed")
#' md <- bb_metadata(client, ex_bed$id, "bed", TRUE)
#' .get_file_path(md$files$bed_file$access_methods[[1]]$access_url$url, "bed")
.get_file_path <- function(file_url, file_type, quietly = FALSE) {
    gzipfile <- .download_and_cache(file_url, quietly)
    tryCatch(
        gunzip(gzipfile, remove = FALSE),
        error = function(e) {
            gsub(".gz", "", gzipfile)
        })
}

#' Return a named vector with type
#'
#' @param file_path character() path to BED
#' @param x double() the x in BEDX+Y
#' @param y double() the y in BEDX+Y
#'
#' @return vector representing extraCols for rtracklayer
#'
#' @examples
#' id <- "608827efc82fcaa4b0bfc65f590ffef8"
#' md <- bb_metadata(client, id, "bed", TRUE)
#' file_path <- .get_file_path(md$files$bed_file$access_methods[[1]]$access_url$url,
#'                             "bed")
#' .get_extra_cols(file_path, 3, 9)
.get_extra_cols <- function(file_path, x, y) {
    t <- read.table(file_path)
    extra_cols <- c()
    stopifnot(x + y == dim(t)[2])
    t_seq <- seq(from = x+1, to = x+y)
    for (i in t[t_seq]) {
        if (typeof(i) == "integer")
            col_type <- "numeric"
        else
            col_type <- typeof(i)
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
#' It will also attempt to supply the `genome` if `genome_alias` exists in the
#' metadata.
#'
#' @param file_path character() path to BED file
#' @param metadata list() full metadata
#' @param extra_cols character() (defaults to NULL) extra column names to
#'        construct GRanges objects
#' @param quietly boolean() (default FALSE) Display information messages
#'
#' @importFrom rlang abort inform
#' @importFrom rtracklayer import
#' @importFrom stringr str_replace str_split_1
#'
#' @return GRanges() object representing BED
#'
#' @examples
#' client <- BEDbase()
#' ex_bed <- bb_example(client, "bed")
#' .bed_file_to_granges(file_path, bed_type, bed_format)
.bed_file_to_granges <- function(file_path, metadata, extra_cols = NULL,
                                 quietly = FALSE) {
    bed_format <- metadata$bed_format
    bed_type <- metadata$bed_type
    nums <- str_replace(bed_type, "bed", "") |>
            str_split_1("\\+") |>
            as.double()

    if (is.null(extra_cols)) {
        extra_cols <- c()
    } else if ((length(extra_cols) > 0) && (nums[2] != length(extra_cols))) {
        abort(paste("The length of `extra_cols` must match the Y value in the",
                    "`bed_type` or be a vector length zero."))
    }

    if (bed_type == "bed12+3") {
        bed_format <- "gappedPeak"
        extra_cols <- c(signalValue = "numeric", pValue = "numeric",
                        qValue = "numeric")
    } else if ((bed_format == "broadpeak" && bed_type == "bed6+3") ||
               (bed_format == "narrowpeak" && bed_type == "bed6+4")) {
        bed_format <- gsub("peak", "Peak", bed_format)
    } else if (bed_format != "broadpeak" && bed_type == "bed6+3") {
        bed_format <- "RNA elements"
        extra_cols <- c(level = "character", signif = "character",
                        score2 = "numeric")
    } else if (nums[2] != length(extra_cols)) {
        bed_format <- "nonstandard"
        extra_cols <- .get_extra_cols(file_path, nums[1], nums[2])
    }

    if (!quietly && bed_format != "bed") {
        inform(paste("Detected", bed_format, "BED file."))
        if (bed_format == "nonstandard") {
            inform(paste("Detecting column and types. Assigning random",
                         "column names. Use `extra_cols` to set the",
                         "name and column type."))
        }
    }

    if (bed_format %in% c("broadPeak", "narrowPeak")) {
        import(file_path, format = bed_format)
    } else if (!is.null(metadata$genome_alias)) {
        tryCatch({
            import(file_path, format = "bed", extraCols = extra_cols,
                   genome = metadata$genome_alias)
        }, error = function(e) {
            import(file_path, format = "bed", extraCols = extra_cols)
        })
    } else {
        import(file_path, format = "bed", extraCols = extra_cols)
    }
}
