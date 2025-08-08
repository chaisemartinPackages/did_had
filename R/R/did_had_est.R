#' Inner estimation function for DIDHAD package
#' @param df A data.frame object
#' @param Y_diff Differenced outcome variable
#' @param group Group Variable
#' @param D Treatment variable
#' @param level level
#' @param kernel kernel
#' @param bw_method bw_method
#' @param yatchew yatchew
#' @param placebo placebo
#' @param dynamic dynamic
#' @importFrom stats lm qnorm as.formula
#' @importFrom nprobust lprobust
#' @importFrom dplyr %>% group_by mutate cur_group_id ungroup lag lead
#' @import YatchewTest
#' @returns Dynamic estimation results (with two periods).
#' @noRd
did_had_est <- function(
    df,
    Y_XX,
    group_XX, 
    D_XX,
    level,
    kernel,
    bw_method,
    yatchew,
    placebo = FALSE,
    dynamic
) {

    df$Y_diff_XX <- df[[Y_XX]]
    df <- subset(df, !is.na(df$Y_diff_XX) & !is.na(df$D_XX))

    # QUG Test
    D_2_vec <- sort(subset(df, df$D_XX > 0)$D_XX)
    t_np <- D_2_vec[1]/(D_2_vec[2] - D_2_vec[1])
    # Test statistic converges in distribution to (E_1/E_2) where E_1 and E_2 are iid random variables from an Exponential(1) distribution
    # This yields the CDF P(T<t) = (\alpha)/(\alpha + (\beta/t)) where \alpha and \beta are the parameters of the two exponential distributions, so in this case both are 1
    np_qug_test <- c(t_np, 1-(1/(1+(1/t_np))))
    names(np_qug_test) <- c("T", "p-value")
    D_2_vec <- t_np <- NULL

    for (j in c(2,3,4)) {
        df[[paste0("D_",j,"_XX")]] <- df$D_XX^j
    }
    df$Y_diff_2_XX <- df$Y_diff_XX^2

    lpres <- lprobust(y = as.vector(df$Y_diff_XX), x = as.vector(df$D_XX), eval = c(0), kernel = kernel, bwselect = bw_method)

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
    if (isTRUE(dynamic)) {
        mean_D_XX <- mean(df$cumulative_XX, na.rm = TRUE)
    } else {
        mean_D_XX <- mean(df$D_XX, na.rm = TRUE)
    }
    ret$beta_qs_XX <- (mean_Y_diff_XX - mu_hat_XX_alt) / mean_D_XX
    B_hat_Hg_XX <- M_hat_hG_XX/mean_D_XX
    ret$se_naive_XX <- se_mu_XX/mean_D_XX
    alpha <- 1 - (level)/2
    ret$low_XX <- ret$beta_qs_XX - B_hat_Hg_XX - qnorm(alpha) * ret$se_naive_XX
    ret$up_XX <- ret$beta_qs_XX - B_hat_Hg_XX + qnorm(alpha) * ret$se_naive_XX
    df$count <- as.numeric(df$D_XX <= ret$h_star)
    ret$within_bw_XX <- sum(df$count, na.rm = TRUE)
    df$count <- NULL
    ret$np_qug_test <- np_qug_test

    if (isTRUE(yatchew)) {
        ret$yt_res <- yatchew_test(data = df, Y = "Y_diff_XX", D = "D_XX", het_robust = TRUE, order = 1-as.numeric(placebo))$results
    }

    return(ret)
}