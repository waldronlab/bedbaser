#' Format BED file metadata
#'
#' @param records list() metadata records
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map_depth
#' @importFrom tidyr unnest_wider
#'
#' @return tibble() file metadata
#'
#' @examples
#' client <- BEDbase()
#' example <- content(client$get_example_bed_record_v1_bed_example_get())
#' .format_metadata_files(example$files)
.format_metadata_files <- function(records) {
    bind_rows(records) |>
        unnest_wider(access_methods) |> unnest_wider(access_url)
}

#' Return a named vector with type
#'
#' @param file_path character() path to BED
#' @param y double() the y in BEDX+Y
#'
#' @return vector representing extraCols for rtracklayer
#'
#' @examples
#'
#' .get_extraCols("path/to/my/file.bed", 3)
.get_extraCols <- function(file_path, y) {
    t <- read.table(file_path)
    extraCols <- c()
    for (i in t[y:dim(t)[2]])
        extraCols <- c(extraCols, typeof(i))
    extraCols <- setNames(extraCols, names(t[y:dim(t)[2]]))
    extraCols
}

#' Create GRanges object from a BED file
#'
#' If the BED format is known, `extra_cols` may be used to set the column name
#' and type. For example, `extra_cols = c(signalValue = "numeric",
#' pValue = "numeric", qValue = "numeric")`.
#'
#' @param file_path character() path to BED file
#' @param bed_type character() bed type
#' @param bed_format character() format name
#' @param extra_cols character() extra column names to construct GRanges objects
#' @param quietly boolean() (default FALSE) Display information messages
#'
#' @importFrom rtracklayer import.bed
#' @importFrom stringr str_replace str_split_1
#'
#' @return GRanges() object representing BED
#'
#' @examples
#' .file_to_granges(file_path, bed_type, bed_format)
.file_to_granges <- function(file_path, bed_type, bed_format, extra_cols,
                             quietly = FALSE) {
    nums <- str_replace(bed_type, "bed", "") |> str_split_1("\\+")

    if (!identical(c(), extra_cols))
        extraCols <- extra_cols
    else if (bed_type == "bed12+3") {
        if (!quietly)
            inform("Detected bed12+3.")
        extraCols <- c(signalValue = "numeric", pValue = "numeric",
                       qValue = "numeric")
    } else if (bed_format == "narrowpeak" && bed_type == "bed6+4") {
        if (!quietly)
            inform(paste("Detected", bed_format, "."))
        extraCols <- c(signalValue = "numeric", pValue = "numeric",
                       qValue = "numeric", peak = "numeric")
    } else if (bed_type == "bed6+4") {
        if (!quietly)
            inform(paste("Detected peptidemapping."))
        extraCols <- c(rawScore = "numeric", spectrumId = "numeric",
                       peptideRank = "numeric", peptideRepeatCount = "numeric")
    } else if (bed_format == "broadpeak" && bed_type == "bed6+3") {
        if (!quietly)
            inform(paste("Detected", bed_format, "."))
        extraCols <- c(signalValue = "numeric", pValue = "numeric",
                       qValue = "numeric")
    } else if (bed_type == "bed6+2") {
        if (!quietly)
            inform(paste("Detected pairedtagalign."))
        extraCols <- c(strand = "character", seq1 = "numeric", seq2 = "numeric")
    } else if (bed_type == "bed3+3") {
        if (!quietly)
            inform(paste("Detected tagAlign."))
        extraCols <- c(sequence = "numeric", score = "numeric",
                       strand = "character")
    } else if (nums[2] == "0") {
        extraCols <- c()
    } else {
        if (!quietly)
            inform(paste("Detected nonstandard BED. Detecting column",
                         "types and assigning random column names. Use",
                         "extra_cols to set the name and column type."))
        extraCols <- .get_extraCols(file_path,
                                    as.double(nums[1]) + as.double(nums[2]))
    }
    import.bed(file_path, extraCols = extraCols)
}
