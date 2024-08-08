#' BEDbase class
#'
#' @importFrom methods new
.BEDbase <- setClass(
    "BEDbase",
    contains = "Service"
)

.BEDBASE_API_REFERENCE_VERSION <- "0.5.0"
