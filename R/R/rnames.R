#' rnames method for did_had
#' @name rnames.did_had
#' @param obj obj
#' @param ... Undefined
#' @import rnames
#' @export
#' @noRd
rnames.did_had <- function(obj, ...) {
    class(obj) <- "list"
    return(rnames(obj))
}