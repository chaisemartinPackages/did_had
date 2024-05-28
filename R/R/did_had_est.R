#' Inner estimation function for DIDHAD package
#' @param df A data.frame object
#' @param Y_diff Differenced outcome variable
#' @param group Group Variable
#' @param D Treatment variable
#' @param level level
#' @param kernel kernel
#' @importFrom stats lm qnorm as.formula
#' @import dplyr
#' @import lpdensity
#' @noRd
did_had_est <- function(
    df,
    Y_diff_XX,
    group_XX, 
    D_XX,
    level,
    kernel
) {

    df <- subset(df, !is.na(df$Y_diff_XX) & !is.na(df$D_XX))

    for (j in c(2,3,4)) {
        df[[paste0("D_",j,"_XX")]] <- df$D_XX^j
    }
    df$Y_diff_2_XX <- df$Y_diff_XX^2
}