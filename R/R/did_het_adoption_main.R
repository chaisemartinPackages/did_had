#' Inner main function for DIDHetAdoption package
#' @param df A data.frame object
#' @param outcome Outcome variable
#' @param treatment Treatment variable
#' @param group Group Variable
#' @param effects effects
#' @param placebo placebo
#' @param level level
#' @importFrom plm pdata.frame make.pbalanced
#' @importFrom stats lm qnorm as.formula
#' @import dplyr
#' @noRd
did_het_adoption_main <- function(
    df,
    outcome,
    group,
    time,
    treatment,
    effects,
    placebo,
    level
) {

    df <- df[c(outcome, group, time, treatment)]
    short_vars <- list(outcome = "Y", treatment = "D", group = "group", time = "time")
    for (v in c("outcome", "treatment", "group", "time")) {
        df <- subset(df, !is.na(df[[get(v)]]))
        if (v %in% c("outcome", "treatment")) {
            df[[paste0(short_vars[[v]],"_XX")]] <- as.numeric(df[[get(v)]])
        } else {
            df <- df %>% group_by(.data[[get(v)]]) %>% 
                mutate(!!paste0(short_vars[[v]],"_XX") := cur_group_id()) %>% ungroup()
        }
    }

    df <- df[order(df$group_XX, df$time_XX), ]
    df <- pdata.frame(df, index = c("group_XX", "time_XX")) 
    df <- make.pbalanced(df, balance.type = "fill")
    df$time_XX <- as.numeric(df$time_XX)
    df$group_XX <- as.numeric(df$group_XX)
    rownames(df) <- 1:nrow(df)

    t_min_XX <- min(df$time_XX, na.rm = TRUE)
    T_max_XX <- max(df$time_XX, na.rm = TRUE)

    df$F_g_temp_XX <- df$time_XX * as.numeric((df$D_XX != lag(df$D_XX) & df$time_XX != 1))
    df$F_g_temp_XX <- ifelse(df$F_g_temp_XX == 0, T_max_XX + 1, df$F_g_temp_XX)
    df <- df %>% group_by(.data$group_XX) %>% mutate(F_g_XX = min(.data$F_g_temp_XX, na.rm = TRUE)) %>% ungroup()
    df$F_g_temp_XX <- NULL

    if (sd(df$F_g_XX, na.rm = TRUE) != 0) {
        stop("Not all groups change their treatment at the same period for the first time. The estimator from de Chaisemartin & D'Haultfoeuille (2024) is only valid if this condition is met.")
    }
    F_XX <- mean(df$F_g_XX, na.rm = TRUE)
    l_XX <- T_max_XX - F_XX + 1

    if (effects > l_XX) {
        message(sprintf("The number of effects requested is too large. The number of effects which can be estimated is at most %.0f.	The command will therefore try to estimate %0.f effect(s).", l_XX, l_XX))
        effects <- l_XX
    }
    l_placebo_XX <- F_XX - 2

    if (placebo != 0) {
        if (l_placebo_XX < placebo & effects >= placebo) {
            message(sprintf("The number of placebos which can be estimated is at most %.0f. The command will therefore try to estimate %.0f placebo(s).", l_placebo_XX, l_placebo_XX))
            placeno <- l_placebo_XX
        }
        if (effects < placebo) {
            message(sprintf("The number of placebo requested cannot be larger than the number of effects requested. The command cannot compute more than %.0f placebo(s).", effects))
            placebo <- min(l_placebo_XX, effects)
        }
    }

    df <- df[order(df$group_XX, df$time_XX), ]
    
    if (placebo != 0) {
        for (i in 1:placebo) {
            df$D_XX <- ifelse(df$time_XX == df$F_g_XX - 1 - i,  lead(df$D_XX, 2*i), df$D_XX)
        }
    }

    View(df)
    stop()


    df_temp <- df[c("diff_Y_XX", "D_XX")]
    names(df_temp) <- c("diff_Y", "D")
    names(h_star_scalars) <- c("f2_prime", "sigma2", "fd_zero", "N_BW")

    out <- list(mat_res_XX, list(df_temp), h_star_scalars)
    names(out) <- c("mat", "data", "h_scalars")
    return(out)
}