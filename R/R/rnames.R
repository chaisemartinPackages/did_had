#' rnames method for did_had
#' @name rnames.did_had
#' @param obj obj
#' @param ... Undefined
#' @import rnames
#' @returns The sub-list map of a did_had object.
#' @export
#' @noRd
rnames.did_had <- function(obj, ...) {
    class(obj) <- "list"
    return(rnames(obj))
}