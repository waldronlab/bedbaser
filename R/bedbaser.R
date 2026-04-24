#' BEDbase class
#'
#' @importFrom methods new
#'
#' @return BEDbase class instance
.BEDbase <- setClass(
    "BEDbase",
    slots = c("bedfiles", "bedsets"),
    contains = "Service"
)

.BEDBASE_API_REFERENCE_VERSION <- "0.12.7"

#' @rdname BEDbase
#'
#' @title An R client for BEDbase
#'
#' @description bedbaser exposes the [bedhost API](https://api.bedbase.org)
#' and includes convenience functions for common tasks, such as to import a
#' BED file by `id` into a GRanges object and a BEDset by its `id` into a
#' GRangesList.
#'
#' @details \code{BEDbase()} creates a cache similar to that of the
#' [Geniml BBClient's cache](https://docs.bedbase.org/geniml).
#'
#' The convenience functions are as follows
#' * `bedbaser::BEDbase()`: API service constructor
#' * `bedbaser::getCache()`: Retrieve cache
#' * `bedbaser::setCache()`: Set path to cache
#' * `bedbaser::bb_stats()`: Retrieve BEDbase statistics
#' * `bedbaser::bb_example()`: Retrieve an example BED file or BEDset
#' * `bedbaser::bb_metadata()`: Retrieve metadata for a BED file or BEDset
#' * `bedbaser::bb_list_beds()`: List all BED files
#' * `bedbaser::bb_list_bedsets()`: List all BEDsets
#' * `bedbaser::bb_beds_in_bedset()`: List BED files in BEDset
#' * `bedbaser::bb_bed_text_search()`: Search BED files by text
#' * `bedbaser::bb_to_granges()`: Create a GRanges object from a BED id
#' * `bedbaser::bb_to_grangeslist()`: Create a GRangesList from a BEDset id
#' * `bedbaser::bb_save()`: Save a BED file to a path.
#'
#' @param cache_path character(1) cache
#' @param quietly logical(1) (default \code{FALSE}) display messages
#'
#' @return BEDbase object
#'
#' @examples
#' bedbase <- BEDbase(cache_path = tempdir())
#' ex_bed <- bb_example(bedbase, "bed")
#' bb_metadata(bedbase, ex_bed$id)
#'
#' @export
BEDbase <- function(cache_path, quietly = FALSE) {
    if (missing(cache_path)) {
        cache_path <- tools::R_user_dir("bedbaser", which = "cache")
    }
    bedfiles_path <- file.path(cache_path, "bedfiles")
    bedsets_path <- file.path(cache_path, "bedsets")
    bedbase <- suppressWarnings(
        .BEDbase(
            bedfiles = BiocFileCache::BiocFileCache(bedfiles_path, ask = FALSE),
            bedsets = BiocFileCache::BiocFileCache(bedsets_path, ask = FALSE),
            AnVIL::Service(
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
    info <- httr::content(
        bedbase$get_bedbase_db_stats_v1_stats_get()
    )
    if (!quietly) {
        message(info$bedfiles_number, " BED files available.")
    }
    bedbase
}

#' @rdname BEDbase
#'
#' @param x BEDbase() object
#' @param cache_type character(1) bedfiles or bedsets
#'
#' @export
setGeneric(
    "getCache",
    function(x, cache_type = c("bedfiles", "bedsets")) {
        standardGeneric("getCache")
    }
)

#' Return cache path
#'
#' @param x BEDbase() object
#' @param cache_type character(1) bedfiles or bedsets
#'
#' @return BiocFileCache() object of BED files
#'
#' @examples
#' bedbase <- BEDbase(tempdir())
#' getCache(bedbase, "bedfiles")
#'
#' @export
setMethod(
    "getCache", "BEDbase",
    function(x, cache_type = c("bedfiles", "bedsets")) {
        cache_type <- match.arg(cache_type)
        if (cache_type == "bedsets") {
            x@bedsets
        } else {
            x@bedfiles
        }
    }
)

#' @rdname BEDbase
#'
#' @param x BEDbase() object
#' @param cache_path character(1)
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @export
setGeneric(
    "setCache",
    function(x, cache_path, quietly = TRUE) standardGeneric("setCache")
)

#' Set cache along path
#'
#' @description Create a cache for BED files and BEDsets like
#' [Geniml BBClient's cache](https://docs.bedbase.org/geniml).
#'
#' @param x BEDbase() object
#' @param cache_path character(1)
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return [BiocFileCache()][BiocFileCache::BiocFileCache-class] object of BED
#' files
#'
#' @examples
#' bedbase <- BEDbase(tempdir())
#' bedbase <- setCache(bedbase, "/tmp")
#'
#' @export
setMethod(
    "setCache", "BEDbase",
    function(x, cache_path, quietly = TRUE) {
        x@bedfiles <- BiocFileCache::BiocFileCache(
            file.path(cache_path, "bedfiles"),
            !quietly
        )
        x@bedsets <- BiocFileCache::BiocFileCache(
            file.path(cache_path, "bedsets"),
            !quietly
        )
        x
    }
)

#' Display API functions
#'
#' @description Display functions defined through the
#' [bedhost API](https://api.bedbase.org) and their corresponding parameters.
#'
#' @param x BEDbase() object
#' @param ... other options
#' @param .deprecated (default \code{FALSE}) if deprecated
#'
#' @importFrom AnVIL operations
#'
#' @return list() API endpoints
#'
#' @examples
#' bedbase <- BEDbase()
#' operations(bedbase)
#'
#' @export
setMethod(
    "operations", "BEDbase",
    function(x, ..., .deprecated = FALSE) {
        methods::callNextMethod(x, ..., .deprecated = .deprecated)
    }
)

#' Display bedhost API schemas
#'
#' @param x BEDbase() object
#'
#' @importFrom AnVIL schemas
#'
#' @return list() API endpoints
#'
#' @examples
#' bedbase <- BEDbase()
#' schemas(bedbase)
#'
#' @export
setMethod(
    "schemas", "BEDbase",
    function(x) {
        methods::callNextMethod(x)
    }
)

#' Display functions for a tag
#'
#' @description Display functions available through the API associated with a
#' tag keyword in [bedhost](https://api.bedbase.org).
#'
#' @param x BEDbase() object
#' @param .tags character() tags for filtering operations
#' @param .deprecated (default \code{FALSE}) if deprecated
#'
#' @importFrom AnVIL tags
#'
#' @return list() API endpoints
#'
#' @examples
#' bedbase <- BEDbase()
#' unique(tags(bedbase)$tag)
#' tags(bedbase, "bedset")
#'
#' @export
setMethod(
    "tags", "BEDbase",
    function(x, .tags, .deprecated = FALSE) {
        methods::callNextMethod(x, .tags, .deprecated = .deprecated)
    }
)

#' Get BEDbase statistics
#'
#' @description Get statistics on available BED files, BEDsets, and genomoes.
#'
#' @param bedbase BEDbase() object
#' @param detailed logical(1) if TRUE display detailed information
#'
#' @return An invisible \code{NULL}
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_stats(bedbase)
#' @export
bb_stats <- function(bedbase, detailed = FALSE) {
    if (detailed) {
        rsp <- bedbase$get_detailed_stats_v1_detailed_stats_get()
    } else {
        rsp <- bedbase$get_bedbase_db_stats_v1_stats_get()
    }
    httr::content(rsp)
}

#' Get the example BED file or BEDset with metadata
#'
#' @description Get the example BED file or BEDset available through
#' [bedhost](https://api.bedbase.org). Useful for an initial exploration
#' of bedbaser with an example BED file and BEDset in BEDbase.
#'
#' @param bedbase BEDbase() object
#' @param rec_type character(1) bed or bedset
#'
#' @return list() bed files or bedsets
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' str(ex_bed)
#' ex_bedset <- bb_example(bedbase, "bedset")
#' str(ex_bedset)
#' @export
bb_example <- function(bedbase, rec_type = c("bed", "bedset")) {
    rec_type <- match.arg(rec_type)
    if (rec_type == "bed") {
        rsp <- bedbase$get_example_bed_record_v1_bed_example_get()
    } else {
        rsp <- bedbase$get_example_bedset_record_v1_bedset_example_get()
    }
    httr::content(rsp)
}

#' Get metadata for a BED file or BEDset
#'
#' @description Get metadata for a BED file or BEDset given its id. Abort if
#' not found or id is not not 32 characters.
#'
#' @rdname bb_metadata
#'
#' @param bedbase BEDbase() object
#' @param id integer(1) record or object identifier
#' @param full logical(1) (default \code{FALSE}) include full record with
#' stats, files, and metadata
#'
#' @return list() metadata
#'
#' @examples
#' bedbase <- BEDbase()
#'
#' ex_bed <- bb_example(bedbase, "bed")
#' bb_metadata(bedbase, ex_bed$id)
#'
#' ex_bedset <- bb_example(bedbase, "bedset")
#' bb_metadata(bedbase, ex_bedset$id)
#'
#' @export
bb_metadata <- function(bedbase, id, full = FALSE) {
    rsp <- bedbase$get_bed_metadata_v1_bed__bed_id__metadata_get(
        bed_id = id,
        full = full,
        test_request = .is_test_request()
    )
    if (rsp$status_code != 200) {
        rsp <- bedbase$get_bedset_metadata_v1_bedset__bedset_id__metadata_get(
            bedset_id = id,
            full = full,
            test_request = .is_test_request()
        )
    }
    result <- httr::content(rsp)
    if (rsp$status_code == 404) {
        rlang::abort(result$detail)
    } else if (rsp$status != 200) {
        rlang::abort("{result$type}: input {result$input} {result$msg}")
    } else {
        result
    }
}

#' List BEDs
#'
#' @description List BED files available through
#' [bedhost](https://api.bedbase.org). By default uses the bedhost default
#' of 1000 records and an initial offset of 0.
#'
#' @rdname bb_list_beds
#'
#' @param bedbase BEDbase() object
#' @param genome character(1) (default \code{NULL}) genome keyword
#' @param bed_compliance character(1) (default \code{NULL}) bed compliance,
#' e.g., 'bed6+4'
#' @param limit integer(1) (default \code{1000}) maximum records
#' @param offset integer(1) (default \code{0}) page token of records
#'
#' @return [tibble][tibble::tibble] of BED records
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_list_beds(bedbase)
#'
#' @export
bb_list_beds <- function(bedbase, genome = NULL, bed_compliance = NULL,
    limit = 1000, offset = 0) {
    rsp <- bedbase$list_beds_v1_bed_list_get(
        genome = genome, bed_compliance = bed_compliance,
        limit = limit, offset = offset
    )
    recs <- httr::content(rsp)
    results <- tibble::tibble()
    if (recs$count) {
        results <- do.call(
            dplyr::bind_rows,
            lapply(recs$results, function(x) {
                unlist(x)
            })
        )
    }
    results
}

#' List BEDsets
#'
#' @description List BEDsets available through
#' [bedhost](https://api.bedbase.org). By default uses the bedhost default
#' of 1000 records and an initial offset of 0.
#'
#' @rdname bb_list_bedsets
#'
#' @param bedbase BEDbase() object
#' @param query character() (default \code{""}) keyword
#' @param limit integer(1) (default \code{1000}) maximum records
#' @param offset integer(1) (default \code{0}) page token of records
#'
#' @return [tibble][tibble::tibble] of BEDset records
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_list_bedsets(bedbase)
#'
#' @export
bb_list_bedsets <- function(bedbase, query = "", limit = 1000, offset = 0) {
    rsp <- bedbase$list_bedsets_v1_bedset_list_get(
        query = query,
        limit = limit,
        offset = offset,
        test_request = .is_test_request()
    )
    recs <- httr::content(rsp)
    results <- tibble::tibble()
    by <- c(
        "id", "name", "md5sum", "submission_date", "last_update_date",
        "description", "author", "source"
    )
    if (recs$count) {
        results <- dplyr::bind_rows(recs$results) |>
            tidyr::unnest(bed_ids) |>
            dplyr::group_by(
                id, name, md5sum, submission_date, last_update_date,
                description, author, source
            ) |>
            tidyr::nest(.key = "bed_ids") |>
            dplyr::relocate(bed_ids, .before = "author")
    }
    results
}

#' Get BEDs associated with BEDset
#'
#' @description Return a tibble of BED files in BEDset given its id.
#'
#' @rdname bb_beds_in_bedset
#'
#' @param bedbase BEDbase() object
#' @param bedset_id integer(1) BEDset record identifier
#'
#' @return [tibble][tibble::tibble] of BED files in BEDset
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bedset <- bb_example(bedbase, "bedset")
#' bb_beds_in_bedset(bedbase, ex_bedset$id)
#'
#' @export
bb_beds_in_bedset <- function(bedbase, bedset_id) {
    rsp <- bedbase$get_bedfiles_in_bedset_v1_bedset__bedset_id__bedfiles_get(
        bedset_id = bedset_id
    )
    recs <- httr::content(rsp)
    results <- tibble::tibble()
    if (recs$count) {
        results <- do.call(
            dplyr::bind_rows,
            lapply(recs$results, function(x) {
                unlist(x)
            })
        )
    }
    results
}

#' Search BED files by text
#'
#' @description Return all available BED files ranked by relevance to the
#' keywords. Uses the [bedhost API](https://api.bedbase.org) default of 10
#' records and an initial offset of 0.
#'
#' @rdname bb_bed_text_search
#'
#' @param bedbase BEDbase() object
#' @param query character() keywords to search
#' @param genome character() (default NULL) genome to search
#' @param assay character() (default NULL) assay to search
#' @param limit integer(1) (default \code{10}) maximum number of results
#' @param offset integer(1) (default \code{0}) page offset of results
#'
#' @return [tibble][tibble::tibble] of results
#'
#' @examples
#' bedbase <- BEDbase()
#' bb_bed_text_search(bedbase, "hg38")
#'
#' @export
bb_bed_text_search <- function(bedbase, query, genome = NULL, assay = NULL,
                               limit = 10, offset = 0) {
    query <- utils::URLencode(query, reserved = TRUE)
    if (!is.null(genome))
        genome <- utils::URLencode(genome, reserved = TRUE)
    if (!is.null(assay))
        assay <- utils::URLencode(assay, reserved = TRUE)
    rsp <- bedbase$text_to_bed_search_v1_bed_search_text_get(
        query = query,
        genome = genome,
        assay = assay,
        limit = limit,
        offset = offset,
        test_request = .is_test_request()
    )
    recs <- httr::content(rsp)
    results <- tibble::tibble()
    if (recs$count) {
        results <- purrr::map_depth(.x = recs$results, 1, \(y) unlist(y)) |>
            dplyr::bind_rows()
    }
    results
}

#' Create a GRanges object given a BED id
#'
#' @description Create a GRanges object given a BED id. Columns and types
#' are generated for broad and narrow peak files. Known columns and types can
#' be passed as a named vector through `extra_cols`. Otherwise,
#' `bb_to_granges()` attempts to determine the column type and substitute dummy
#' column names.
#'
#' @rdname bb_to_granges
#'
#' @param bedbase BEDbase() object
#' @param bed_id integer(1) BED record identifier
#' @param extra_cols character() (default \code{NULL}) extra column names to
#' construct GRanges objects
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return [GRanges][GenomicRanges::GRanges-class]
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' bb_to_granges(bedbase, ex_bed$id)
#'
#' @export
bb_to_granges <- function(bedbase, bed_id, extra_cols = NULL, quietly = TRUE) {
    file_path <- .get_file(bedbase, bed_id, getCache(bedbase, "bedfiles"), quietly)

    tryCatch(
        R.utils::gunzip(file_path, remove = FALSE),
        error = function(e) {
            gsub(".gz", "", file_path)
        }
    )

    metadata <- bb_metadata(bedbase, bed_id, TRUE)
    .bed_file_to_granges(file_path, metadata, extra_cols, quietly)
}

#' Create a GRangesList object given a BEDset id
#'
#' @rdname bb_to_grangeslist
#'
#' @param bedbase BEDbase() object
#' @param bedset_id integer(1) BEDset record identifier
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return [GRangesList][GenomicRanges::GRangesList-class]
#'
#' @examples
#' bedbase <- BEDbase()
#' bedset_id <- "lola_hg38_ucsc_features"
#' bb_to_grangeslist(bedbase, bedset_id)
#'
#' @export
bb_to_grangeslist <- function(bedbase, bedset_id, quietly = TRUE) {
    beds <- bb_beds_in_bedset(bedbase, bedset_id)
    gros <- list()
    if (!quietly) {
        rlang::inform(paste(length(beds$id), "BED files in", bedset_id))
    }
    .cache_bedset_txt(bedset_id, beds$id, getCache(bedbase, "bedsets"))
    for (bed_id in beds$id) {
        gro <- bb_to_granges(bedbase, bed_id, quietly = quietly)
        gros[[length(gros) + 1]] <- gro
    }
    GenomicRanges::GRangesList(gros)
}

#' Save a BED file or BEDset to a path given an id
#'
#' @description Save a BED file or a BEDset to a local path. If the path does
#' not exist, `bb_save()` will abort.
#'
#' @rdname bb_save
#'
#' @param bedbase BEDbase() object
#' @param bed_or_bedset_id integer(1) BED or BEDset record identifier
#' @param path character(1) directory to save file
#' @param quietly logical(1) (default \code{TRUE}) display messages
#'
#' @return An invisible \code{NULL}
#'
#' @examples
#' bedbase <- BEDbase()
#' ex_bed <- bb_example(bedbase, "bed")
#' bb_save(bedbase, ex_bed$id, tempdir())
#'
#' @export
bb_save <- function(bedbase, bed_or_bedset_id, path, quietly = TRUE) {
    if (!dir.exists(path)) {
        rlang::abort(paste(path, "doesn't exist.", sep = " "))
    }
    metadata <- bb_metadata(bedbase, bed_or_bedset_id, TRUE)
    if ("bedsets" %in% names(metadata)) {
        ids <- list(metadata$id)
    } else {
        ids <- metadata$bed_ids
        .cache_bedset_txt(
            bed_or_bedset_id, unlist(metadata$bed_ids),
            getCache(bedbase, "bedsets")
        )
    }
    for (id in ids) {
        .get_file(bedbase, id, path, quietly)
    }
}
