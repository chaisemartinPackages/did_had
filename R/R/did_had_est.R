#' Inner estimation function for DIDHAD package
#' @param df A data.frame object
#' @param Y_diff Differenced outcome variable
#' @param group Group Variable
#' @param D Treatment variable
#' @param level level
#' @param kernel kernel
#' @param yatchew yatchew
#' @param placebo placebo
#' @importFrom stats lm qnorm as.formula
#' @importFrom nprobust lprobust
#' @import dplyr
#' @import YatchewTest
#' @noRd
did_had_est <- function(
    df,
    Y_XX,
    group_XX, 
    D_XX,
    level,
    kernel,
    yatchew,
    placebo = FALSE
) {

    df$Y_diff_XX <- df[[Y_XX]]
    df <- subset(df, !is.na(df$Y_diff_XX) & !is.na(df$D_XX))

    for (j in c(2,3,4)) {
        df[[paste0("D_",j,"_XX")]] <- df$D_XX^j
    }
    df$Y_diff_2_XX <- df$Y_diff_XX^2

    lpres <- lprobust(y = as.vector(df$Y_diff_XX), x = as.vector(df$D_XX), eval = c(0), kernel = kernel)

    ret <- list()
    ret$h_star <- lpres$Estimate[1,2]
    mu_hat_XX_alt <- lpres$Estimate[1,5]
    mu_hat_XX_alt_ub <- lpres$Estimate[1,6]
    M_hat_hG_XX <- lpres$Estimate[1,6] - lpres$Estimate[1,5]
    se_mu_XX <- lpres$Estimate[1,8]
    coverage_mu_XX <- as.numeric(mu_hat_XX_alt_ub - 1.96 * se_mu_XX <= 0 &
        mu_hat_XX_alt_ub + 1.96 * se_mu_XX >= 0)
    ret$G_XX <- max(df$group_XX, na.rm = TRUE)
    mean_Y_diff_XX <- mean(df$Y_diff_XX, na.rm = TRUE)
    mean_D_XX <- mean(df$D_XX, na.rm = TRUE)
    ret$beta_qs_XX <- (mean_Y_diff_XX - mu_hat_XX_alt) / mean_D_XX
    B_hat_Hg_XX <- M_hat_hG_XX/mean_D_XX
    ret$se_naive_XX <- se_mu_XX/mean_D_XX
    alpha <- 1 - (level)/2
    ret$low_XX <- ret$beta_qs_XX - B_hat_Hg_XX - qnorm(alpha) * ret$se_naive_XX
    ret$up_XX <- ret$beta_qs_XX - B_hat_Hg_XX + qnorm(alpha) * ret$se_naive_XX
    df$count <- as.numeric(df$D_XX <= ret$h_star)
    ret$within_bw_XX <- sum(df$count, na.rm = TRUE)
    df$count <- NULL

    if (isTRUE(yatchew)) {
        ret$yt_res <- yatchew_test(data = df, Y = "Y_diff_XX", D = "D_XX", het_robust = TRUE, order = 1-as.numeric(placebo))$results
    }

    return(ret)
}