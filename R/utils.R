#' Construct object identifier
#'
#' @param rec_id character() BEDbase record identifier
#' @param rec_type character() (default bed) BEDbase record type
#' @param result_id character() (default bedfile) BEDbase result identifier
#'
#' @return String
#'
#' @examples
#' obj_id <- make_obj_id("bbad85f21962bb8d972444f7f9a3a932")
make_obj_id <- function(rec_id, rec_type = "bed", result_id = "bedfile")
{
    paste(rec_type, rec_id, result_id, sep =".")
}

#' Decompress gunzip file
#'
#' @param file_path character() path to gunzip file
#'
#' @importFrom stringr str_replace
#'
#' @return character() decompressed file path
#'
#' @examples
#' .ungzip("path/to/my.gzip")
.ungzip <- function(file_path) {
    system(paste("gzip -d", file_path))
    str_replace(file_path, "\\.gz", "")
}

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
    results <- map_depth(.x = records, 2, ~ replace(.x, is.null(.x), NA),) |>
        bind_rows() |>
        mutate(access_methods = map_depth(access_methods, 2,
                                          ~ replace(.x, is.null(.x), NA))) |>
        unnest_wider(access_methods) |>
        mutate(access_url = map_depth(access_url, 2,
                                      ~ replace(.x, is.null(.x), NA))) |>
        unnest_wider(access_url)
    results
}

#' Format BED file metadata
#'
#' @param file_path character() path to BED file
#' @param bed_type character() bed type
#' @param bed_format character() format name
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom rtracklayer import import.bed
#' @importFrom stringr str_replace
#'
#' @return GRanges()
#'
#' @examples
#' .file_to_granges(file_path, bed_type, bed_format)
.file_to_granges <- function(file_path, bed_type, bed_format) {
    nums <- stringr::str_replace(bed_type, "bed", "")
    if (bed_format == "broadpeak" && bed_type == "bed6+3") {
        extraCols <- c(signalValue = "numeric", pValue = "numeric",
                       qValue = "numeric")
        obj <- import(file_path, format = "BED", extraCols = extraCols)
    } else if (bed_format == "narrowpeak" && bed_type == "bed6+4") {
        extraCols <- c(signalValue = "numeric", pValue = "numeric",
                       qValue = "numeric", peak = "integer")
        obj <- import(file_path, format = "BED", extraCols = extraCols)
    } else if (as.double(num[2]) == 0) {
        obj <- import.bed(file_path)
    } else {
        # extraCols <-
        # obj <- import(f, format = "BED", extraCols = extraCols)
        # t <- read.table(f)
        message("Not yet implemented")
    }
    obj
}
