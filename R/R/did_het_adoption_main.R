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
    level,
    kernel
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

    df$F_g_int_XX <-  as.numeric((df$D_XX != lag(df$D_XX) & df$time_XX != 1 & lag(df$D_XX) == 0))
    df$F_g_temp_XX <- df$time_XX * df$F_g_int_XX
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

    l_placebo_XX <- placebo
    if (placebo != 0) {
        l_placebo_XX <- F_XX - 2
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

    resmat <- matrix(NA, nrow = l_XX + l_placebo_XX, ncol = 8)
    rown<- sapply(1:l_XX, function(x) paste0("Effect_", x))
    coln <- c("Estimate", "SE", "LB.CI", "UB.CI", "N", "BW", "N.BW", "ID")
    for (i in 1:l_XX) {
        df[[paste0("Effect_", i)]] <- ifelse(df$F_g_int_XX == 1, lead(df$Y_XX, i - 1) - lag(df$Y_XX, 1), NA)
        res <- did_had_est(df = df, Y_XX = paste0("Effect_", i), D_XX = "D_XX", group_XX = "group_XX", level = level, kernel = kernel)
        resmat[i, 1] <- res$beta_qs_XX
        resmat[i, 2] <- res$se_naive_XX
        resmat[i, 3] <- res$low_XX
        resmat[i, 4] <- res$up_XX
        resmat[i, 5] <- res$G_XX
        resmat[i, 6] <- res$h_star
        resmat[i, 7] <- res$within_bw_XX
        resmat[i, 8] <- i
    }
    if (placebo != 0) {
        for (i in 1:l_placebo_XX) {
            df[[paste0("Effect_", i)]] <- ifelse(df$F_g_int_XX == 1, lag(df$Y_XX, i+1) - lag(df$Y_XX, 1), NA)
            res <- did_had_est(df = df, Y_XX = paste0("Effect_", i), D_XX = "D_XX", group_XX = "group_XX", level = level, kernel = kernel)
            resmat[l_XX+i, 1] <- res$beta_qs_XX
            resmat[l_XX+i, 2] <- res$se_naive_XX
            resmat[l_XX+i, 3] <- res$low_XX
            resmat[l_XX+i, 4] <- res$up_XX
            resmat[l_XX+i, 5] <- res$G_XX
            resmat[l_XX+i, 6] <- res$h_star
            resmat[l_XX+i, 7] <- res$within_bw_XX
            resmat[l_XX+i, 8] <- -i
        }
        rown <- c(rown, sapply(1:l_placebo_XX, function(x) paste0("Placebo_", x)))
    }

    colnames(resmat) <- coln
    rownames(resmat) <- rown
    out <- list(resmat = resmat, res.effects = l_XX, res.placebo = l_placebo_XX)
    return(out)
}