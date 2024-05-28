#' print method for did_had
#' @name yatchew_test.did_had
#' @param data data
#' @param ... Undefined
#' @import YatchewTest
#' @export
#' @noRd
yatchew_test.did_had <- function(data, het_robust = TRUE, ...) {
    if (isTRUE(data$args$no_data)) {
        stop("yatchew_test method for did_het_adoption requires no_data = FALSE (default) in did_het_adoption.")
    }
    data$data <- as.data.frame(data$data)
    yt <- yatchew_test(data$data, "diff_Y", "D", het_robust = het_robust)
    yt$null <- "E[Y\u2082-Y\u2081|D\u2082] is linear in D\u2082"
    return(yt)
}