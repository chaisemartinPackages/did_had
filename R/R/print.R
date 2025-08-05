#' print method for did_had
#' @name print.did_had
#' @param x x
#' @param ... Undefined
#' @returns No return, custom print method for did_had objects.
#' @export
#' @noRd
print.did_had <- function(x, ...) {

    cat("\n")
    cat(sprintf("Test on Quasi-Untreated: T = %.4f | p-value = %.4f \n", x$results$np_qug_test[1], x$results$np_qug_test[2]))
    cat(noquote(strrep("-", 70)));cat("\n");
    cat(strrep(" ", 25));cat("Effect Estimates");cat("\n");
    cat(noquote(strrep("-", 70)));cat("\n");


    mat <- x$results$resmat[1:x$results$res.effects, ]
    if (!inherits(mat, "matrix")) {
        mat <- t(as.matrix(mat))
        rownames(mat) <- "Effect_1"
    }

    dis <- matrix(data = 0, nrow = nrow(mat), ncol = ncol(mat))
    dis[,1:4] <- sprintf("%s", format(round(mat[,1:4], 5), big.mark=",", scientific=FALSE, trim=TRUE))
    dis[,5] <- sprintf("%s", format(round(mat[,5], 0), big.mark=",", scientific=FALSE, trim=TRUE))
    dis[,6] <- sprintf("%s", format(round(mat[,6], 5), big.mark=",", scientific=FALSE, trim=TRUE))
    dis[,7] <- sprintf("%s", format(round(mat[,7], 0), big.mark=",", scientific=FALSE, trim=TRUE))
    rownames(dis) <- rownames(mat)
    colnames(dis) <- colnames(mat)
    print(noquote(dis[,1:7, drop = FALSE]))

    if (x$results$res.placebo > 0) {
        cat("\n")
        cat(noquote(strrep("-", 70)));cat("\n");
        cat(strrep(" ", 25));cat("Placebo Estimates");cat("\n");
        cat(noquote(strrep("-", 70)));cat("\n");

        mat <- x$results$resmat[(x$results$res.effects+1):nrow(x$results$resmat), ]
        if (!inherits(mat, "matrix")) {
            mat <- t(as.matrix(mat))
            rownames(mat) <- "Placebo_1"
        }

        dis <- matrix(data = 0, nrow = nrow(mat), ncol = ncol(mat))
        dis[,1:4] <- sprintf("%s", format(round(mat[,1:4], 5), big.mark=",", scientific=FALSE, trim=TRUE))
        dis[,5] <- sprintf("%s", format(round(mat[,5], 0), big.mark=",", scientific=FALSE, trim=TRUE))
        dis[,6] <- sprintf("%s", format(round(mat[,6], 5), big.mark=",", scientific=FALSE, trim=TRUE))
        dis[,7] <- sprintf("%s", format(round(mat[,7], 0), big.mark=",", scientific=FALSE, trim=TRUE))
        rownames(dis) <- rownames(mat)
        colnames(dis) <- colnames(mat)
        print(noquote(dis[,1:7, drop = FALSE]))

    }

    if (isTRUE(x$args$yatchew)) {
        cat("\n")
        cat(noquote(strrep("-", 70)));cat("\n");
        cat(strrep(" ", 14));cat("Heteroskedasticity-robust Yatchew Test");cat("\n");
        cat(noquote(strrep("-", 70)));cat("\n");

        mat <- x$results$yatchew_test
        if (!inherits(mat, "matrix")) {
            mat <- t(as.matrix(mat))
            rownames(mat) <- "Effect_1"
        }

        dis <- matrix(data = 0, nrow = nrow(mat), ncol = ncol(mat))
        dis[,1:4] <- sprintf("%s", format(round(mat[,1:4], 5), big.mark=",", scientific=FALSE, trim=TRUE))
        dis[,5] <- sprintf("%s", format(round(mat[,5], 0), big.mark=",", scientific=FALSE, trim=TRUE))
        rownames(dis) <- rownames(mat)
        colnames(dis) <- colnames(mat)
        print(noquote(dis[1:x$results$res.effects,1:5, drop = FALSE]))
        if (x$results$res.placebo > 0) {
            cat("\n")
            print(noquote(dis[(x$results$res.effects+1):nrow(dis),1:5, drop = FALSE]))
        }
    }

    cat("\nThe development of this package was funded by the European Union (ERC, REALLYCREDIBLE,GA N. 101043899).\n")
}

#' summary method for did_had
#' @name summary.did_had
#' @param object obj
#' @param ... Undefined
#' @returns No return, custom summary method for did_had objects.
#' @export
#' @noRd
summary.did_had <- function(object, ...) {
    print.did_had(object)
}
