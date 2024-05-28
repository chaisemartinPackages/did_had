#' print method for did_had
#' @name print.did_had
#' @param x x
#' @param ... Undefined
#' @export
#' @noRd
print.did_had <- function(x, ...) {

    cat(noquote(strrep("-", 70)));cat("\n");
    cat(strrep(" ", 2));cat("Estimation of treatment effect in a heterogeneous adoption design");cat("\n");
    cat(noquote(strrep("-", 70)));cat("\n");

    mat <- x$results
    dis <- matrix(data = 0, nrow = nrow(mat), ncol = ncol(mat))
    dis[,1:4] <- sprintf("%s", format(round(mat[,1:4], 5), big.mark=",", scientific=FALSE, trim=TRUE))
    dis[,5] <- sprintf("%s", format(round(mat[,5], 0), big.mark=",", scientific=FALSE, trim=TRUE))
    dis[,6] <- sprintf("%s", format(round(mat[,6], 5), big.mark=",", scientific=FALSE, trim=TRUE))
    dis[,7] <- sprintf("%s", format(round(mat[,7], 0), big.mark=",", scientific=FALSE, trim=TRUE))
    rownames(dis) <- rownames(mat)
    colnames(dis) <- colnames(mat)
    print(noquote(dis[,1:7, drop = FALSE]))
}

#' print method for did_had
#' @name summary.did_had
#' @param object obj
#' @param ... Undefined
#' @export
#' @noRd
summary.did_had <- function(object, ...) {
    print.did_had(object)
}
