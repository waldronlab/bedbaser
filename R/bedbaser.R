#' BEDbase class
#'
#' @importFrom methods new
#'
#' @return BEDbase class instance
.BEDbase <- setClass(
    "BEDbase",
    slots = c("cache"),
    contains = "Service"
)

.BEDBASE_API_REFERENCE_VERSION <- "0.5.0"

#' @rdname BEDbase
#'
#' @title An R client for BEDbase
#'
#' @description bedbaser exposes the BEDbase API and includes convenience
#' functions for common tasks, such as to import a BED file by id into a
#' `GRanges` object and a BEDset by its id into a `GRangesList`.
#'
#' @details
#'
#' The convenience functions are as follows
#' * `bedbaser::BEDbase()`: API service constructor
#' * `bedbaser::getCache()`: Retrieve cache
#' * `bedbaser::setCache()`: Set path to cache
#' * `bedbaser::bb_example()`: Retrieve an example BED file or BEDset
#' * `bedbaser::bb_metadata()`: Retrieve metadata for a BED file or BEDset
#' * `bedbaser::bb_list_beds()`: List all BED files
#' * `bedbaser::bb_list_bedsets()`: List all BEDsets
#' * `bedbaser::bb_beds_in_bedset()`: List BED files in BEDset
#' * `bedbaser::bb_bed_text_search()`: Search BED files by text
#' * `bedbaser::bb_to_granges()`: Create a `GRanges` object from a BED id
#' * `bedbaser::bb_to_grangeslist()`: Create a `GrangesList` from a BEDset id
#' * `bedbaser::bb_save()`: Save a BED file to a path.
#'
#' @param cache_path string() cache
#'
#' @importFrom AnVIL Service
#' @importFrom rlang warn
#'
#' @return BEDbase object
#'
#' @examples
#' BEDbase()
#'
#' @export
BEDbase <- function(cache_path) {
    if (missing(cache_path))
        cache_path <- tools::R_user_dir("bedbaser", which = "cache")
    suppressWarnings(
        .BEDbase(
            cache = BiocFileCache::BiocFileCache(cache_path),
            Service(
                service = "bedbase",
                host = "api.bedbase.org",
                api_reference_version = .BEDBASE_API_REFERENCE_VERSION,
                authenticate = FALSE,
                package = "bedbaser",
                api_url = character(),
                api_reference_url = "https://api.bedbase.org/openapi.json",
            )
        )
    )
}

#' @rdname BEDbase
#'
#' @param x BEDbase object
#' @param quietly (default TRUE) display messages
#'
#' @export
setGeneric("getCache", function(x, quietly = TRUE) standardGeneric("getCache"))

#' Return cache path
#'
#' @param x BEDbase object
#' @param quietly (default TRUE) display messages
#'
#' @return BiocFileCache cache of BED files
#'
#' @examples
#' api <- BEDbase(tempdir())
#' getCache(api)
#'
#' @export
setMethod(
    "getCache", "BEDbase",
    function(x, quietly = TRUE) {
        if (quietly)
            BiocFileCache::bfcinfo(x@cache)
         x@cache
    }
)

#' @rdname BEDbase
#'
#' @param x BEDbase object
#' @param cache_path character()
#' @param quietly (default TRUE) display messages
#'
#' @export
setGeneric("setCache",
    function(x, cache_path, quietly = TRUE) standardGeneric("setCache")
)

#' Set cache path
#'
#' @param x BEDbase object
#' @param cache_path character()
#' @param quietly (default TRUE) display messages
#'
#' @return BiocFileCache cache of BED files
#'
#' @examples
#' api <- BEDbase(tempdir())
#' api <- setCache(api, "/tmp")
#'
#' @export
setMethod(
    "setCache", "BEDbase",
    function(x, cache_path, quietly = TRUE) {
        x@cache <- BiocFileCache::BiocFileCache(cache_path)
        if (quietly)
            BiocFileCache::bfcinfo(x@cache)
        x
    }
)

#' Display API
#'
#' @param x BEDbase object
#' @param ... other options
#' @param .deprecated (default FALSE) if deprecated
#'
#' @importFrom AnVIL operations
#' @importFrom methods callNextMethod
#'
#' @return list() API end points
#'
#' @examples
#' api <- BEDbase()
#' operations(api)
#'
#' @export
setMethod(
    "operations", "BEDbase",
    function(x, ..., .deprecated = FALSE) {
        callNextMethod(x, ..., .deprecated = .deprecated)
    }
)

#' Get the example BED file or BEDset with metadata
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param rec_type character() bed or bedset
#'
#' @importFrom httr content
#'
#' @return list() bed or bedset
#'
#' @examples
#' api <- BEDbase()
#' bb_example(api, "bed")
#' bb_example(api, "bedset")
#'
#' @export
bb_example <- function(api, rec_type = c("bed", "bedset")) {
    rec_type <- match.arg(rec_type)
    if (rec_type == "bed") {
        rsp <- api$get_example_bed_record_v1_bed_example_get()
    } else {
        rsp <- api$get_example_bedset_record_v1_bedset_example_get()
    }
    content(rsp)
}

#' Get metadata for a BED file or BEDset
#'
#' @description Get metadata for a BED file or BEDset. Abort
#' if not found or id is not not 32 characters
#'
#' @rdname bb_metadata
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param id integer() record or object identifier
#' @param full logical() (default FALSE) include full record with stats, files,
#' and metadata
#'
#' @importFrom httr content
#' @importFrom rlang abort
#'
#' @return list() metadata
#'
#' @examples
#' api <- BEDbase()
#'
#' ex_bed <- bb_example(api, "bed")
#' bb_metadata(api, ex_bed$id)
#'
#' ex_bedset <- bb_example(api, "bedset")
#' bb_metadata(api, ex_bedset$id)
#'
#' @export
bb_metadata <- function(api, id, full = FALSE) {
    rsp <- api$get_bed_metadata_v1_bed__bed_id__metadata_get(
        bed_id = id,
        full = full
    )
    if (rsp$status_code != 200) {
        rsp <- api$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(
            bedset_id = id,
            full = full
        )
    }
    result <- content(rsp)
    if (rsp$status_code == 404) {
        abort(message = result$detail)
    } else if (rsp$status != 200) {
        abort(message = "{result$type}: input {result$input} {result$msg}")
    } else {
        result
    }
}

#' List BEDs
#'
#' @rdname bb_list_beds
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param genome character() (default NULL) genome keyword
#' @param bed_type character() (default NULL) bed file type
#' @param limit integer() (default 1000) maximum records
#' @param offset integer() (default 0) page token of records
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#'
#' @return tibble() of BED records
#'
#' @examples
#' api <- BEDbase()
#' bb_list_beds(api)
#'
#' @export
bb_list_beds <- function(
        api, genome = NULL, bed_type = NULL, limit = 1000,
        offset = 0) {
    rsp <- api$list_beds_v1_bed_list_get(
        genome = genome, bed_type = bed_type,
        limit = limit, offset = offset
    )
    recs <- content(rsp)
    results <- tibble()
    if (recs$count) {
        results <- map_depth(
            .x = recs$results, 2,
            ~ replace(.x, is.null(.x), NA)
        ) |>
            bind_rows()
    }
    results
}

#' List BEDsets
#'
#' @rdname bb_list_bedsets
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param query character() (default NULL) keyword
#' @param limit integer() (default 1000) maximum records of bedsets
#' @param offset integer() (default 0) page token of records
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#' @importFrom tidyr unnest
#'
#' @return tibble() of BEDset records
#'
#' @examples
#' api <- BEDbase()
#' bb_list_bedsets(api)
#'
#' @export
bb_list_bedsets <- function(api, query = NULL, limit = 1000, offset = 0) {
    rsp <- api$list_bedsets_v1_bedset_list_get(
        query = query,
        limit = limit,
        offset = offset
    )
    recs <- content(rsp)
    results <- tibble()
    if (recs$count) {
        results <- bind_rows(recs$results) |>
            unnest(cols = c(bed_ids))
    }
    results
}

#' Get BEDs associated with BEDset
#'
#' @rdname bb_beds_in_bedset
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param bedset_id integer() BEDset record identifier
#'
#' @importFrom httr content
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#'
#' @return list() BED record identifiers
#'
#' @examples
#' api <- BEDbase()
#' ex_bedset <- bb_example(api, "bedset")
#' bb_beds_in_bedset(api, ex_bedset$id)
#'
#' @export
bb_beds_in_bedset <- function(api, bedset_id) {
    rsp <- api$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(
        bedset_id = bedset_id
    )
    recs <- content(rsp)
    results <- tibble()
    if (recs$count) {
        results <- bind_rows(recs$results)
    }
    results
}

#' Search BED files by text
#'
#' Returns all available results scored.
#'
#' @rdname bb_bed_text_search
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param query character() keywords to search
#' @param limit integer() (default 10) maximum number of results
#' @param offset integer() (default 0) page offset of results
#'
#' @importFrom dplyr bind_rows
#' @importFrom httr content
#' @importFrom purrr map_depth
#' @importFrom tibble tibble
#' @importFrom utils URLencode
#'
#' @return tibble()
#'
#' @examples
#' api <- BEDbase()
#' bb_bed_text_search(api, "hg38")
#'
#' @export
bb_bed_text_search <- function(api, query, limit = 10, offset = 0) {
    encoded_query <- URLencode(query, reserved = TRUE)
    rsp <- api$text_to_bed_search_v1_bed_search_text_post(
        query = encoded_query,
        limit = limit,
        offset = offset
    )
    recs <- content(rsp)
    results <- tibble()
    if (recs$count) {
        results <- map_depth(.x = recs$results, 1, \(y) unlist(y)) |>
            bind_rows()
    }
    results
}

#' Create a GRanges object given a BED id
#'
#' Generates the column and types for broad and narrow peak files.
#'
#' @rdname bb_to_granges
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param bed_id integer() BED record identifier
#' @param file_type character() bed or bigbed
#' @param extra_cols character() (default NULL) extra column names to
#'        construct GRanges objects
#' @param quietly logical() (default TRUE) display messages
#'
#' @importFrom dplyr filter
#' @importFrom R.utils gunzip
#' @importFrom rtracklayer import.bb
#' @importFrom rlang warn
#'
#' @return GRanges() object
#'
#' @examples
#' api <- BEDbase()
#' ex_bed <- bb_example(api, "bed")
#' bb_to_granges(api, ex_bed$id)
#'
#' @export
bb_to_granges <- function(
        api, bed_id, file_type = "bed", extra_cols = NULL,
        quietly = TRUE) {
    stopifnot(file_type %in% c("bed", "bigbed"))
    metadata <- bb_metadata(api, bed_id, TRUE)
    file_path <- .get_file(metadata, file_type, "http", getCache(api), quietly)

    if (file_type == "bed") {
        .bed_file_to_granges(file_path, metadata, extra_cols, quietly)
    } else if (file_type == "bigbed") {
        if (.Platform$OS.type == "windows") {
            warn("This feature does not work on Windows.")
        } else {
            if (quietly) {
                suppressMessages(import.bb(file_path, format = "bigBed"))
            } else {
                import.bb(file_path, format = "bigBed")
            }
        }
    }
}

#' Create a GRangesList object given a BEDset id
#'
#' @rdname bb_to_grangeslist
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param bedset_id integer() BEDset record identifier
#' @param quietly logical() (default TRUE) display messages
#'
#' @importFrom GenomicRanges GRangesList
#'
#' @return GRangesList() object
#'
#' @examples
#' api <- BEDbase()
#' id <- "lola_hg38_ucsc_features"
#' bb_to_grangeslist(api, id)
#'
#' @export
bb_to_grangeslist <- function(api, bedset_id, quietly = TRUE) {
    beds <- bb_beds_in_bedset(api, bedset_id)
    gros <- list()
    for (bed_id in beds$id) {
        gro <- bb_to_granges(api, bed_id, quietly = quietly)
        gros[[length(gros) + 1]] <- gro
    }
    GRangesList(gros)
}

#' Save a BED or BEDset files to a path given an id
#'
#' @rdname bb_save
#'
#' @param api API object of BEDbase created from BEDbase()
#' @param bed_or_bedset_id integer() BED or BEDset record identifier
#' @param path character() directory to save file
#' @param file_type character() (default bed) bed, bigbed, etc.
#' @param access_type character() (default http)  s3 or http
#' @param quietly logical() (default TRUE) display messages
#'
#' @examples
#' api <- BEDbase()
#' ex <- bb_example(api, "bed") 
#' bb_save(api, ex$id, tempdir())
#'
#' @export
bb_save <- function(
        api, bed_or_bedset_id, path, file_type = "bed", access_type = "http",
        quietly = TRUE)
{
    if (!dir.exists(path))
        rlang::abort(paste(path, "doesn't exist.", sep = " "))
    metadata <- bb_metadata(api, bed_or_bedset_id, TRUE)
    if ("bedsets" %in% names(metadata)) {
        ids <- list(metadata$id)
    } else {
        ids <- metadata$bed_ids
    }
    for (id in ids) {
        metadata <- bb_metadata(api, id, TRUE)
        .get_file(metadata, file_type, access_type, path, quietly)
    }
}
