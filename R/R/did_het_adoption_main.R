#' Inner main function for DIDHetAdoption package
#' @param df A data.frame object
#' @param outcome Outcome variable
#' @param treatment Treatment variable
#' @param group Group Variable
#' @param effects effects
#' @param placebo placebo
#' @param level level
#' @param yatchew yatchew
#' @param trends_lin trends_lin
#' @param dynamic dynamic
#' @param kernel kernel
#' @param bw_method bw_method
#' @importFrom plm pdata.frame make.pbalanced
#' @importFrom stats lm qnorm as.formula sd
#' @importFrom rlang :=
#' @importFrom dplyr %>% group_by mutate cur_group_id ungroup lag lead
#' @returns Main estimation results.
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
    kernel,
    bw_method,
    yatchew,
    trends_lin,
    dynamic
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

    ## Modif Diego (05.08.25): Allow to include stayers in the estimation
    df$F_g_int_XX <-  as.numeric((df$D_XX != lag(df$D_XX) & df$time_XX != 1 & df$group_XX == lag(df$group_XX)))
    df$F_g_temp_XX <- df$time_XX * df$F_g_int_XX
    df$F_g_temp_XX <- as.numeric(ifelse(df$F_g_temp_XX == 0, T_max_XX + 1, df$F_g_temp_XX))
    df <- df %>% group_by(.data$group_XX) %>% mutate(F_g_XX = min(.data$F_g_temp_XX, na.rm = TRUE)) %>% ungroup()
    df$F_g_temp_XX <- NULL

    if (sd(subset(df, df$F_g_XX != T_max_XX + 1)$F_g_XX, na.rm = TRUE) != 0) {
        stop("Not all groups change their treatment at the same period for the first time. The estimator from de Chaisemartin & D'Haultfoeuille (2024) is only valid if this condition is met.")
    }

    # Save treatment onset period as a scalar
    F_XX <- mean(subset(df, df$F_g_XX != T_max_XX + 1)$F_g_XX, na.rm = TRUE)

    # Check whether there are stayers
    F_g_set <- subset(df, df$time_XX == F_XX)["F_g_XX"]
    n_stayers <- nrow(subset(F_g_set, F_g_set$F_g_XX == T_max_XX + 1))
    if (n_stayers != 0) {
        message(sprintf("NOTE: %.0f groups are untreated at period 2.", n_stayers))
    }
    F_g_set <- NULL
    df$F_g_XX <- ifelse(df$F_g_XX == T_max_XX + 1, F_XX, df$F_g_XX)

    # Compute max number of effects and placebos
    l_XX <- T_max_XX - F_XX + 1
    l_placebo_XX <- F_XX - 2

    if (isTRUE(trends_lin)) {
        if (F_XX < 3) {
            stop("Your data has less than 3 pre-treatment periods so it is impossible for the command to account for linear trends.")
        }

        # Adjust number of placebos
        l_placebo_XX <- l_placebo_XX - 1

        # Adjust outcome var to account for linear trend
        df <- df[order(df$group_XX, df$time_XX), ]
        df$lin_trend_int_XX <- ifelse(df$time_XX == F_XX - 1, df$Y_XX - lag(df$Y_XX), NA)
        df <- df %>% group_by(.data$group_XX) %>% mutate(lin_trend_XX = mean(.data$lin_trend_int_XX, na.rm = TRUE))  %>% ungroup()
        df$lin_trend_int_XX <- NULL

    }

    if (effects > l_XX) {
        message(sprintf("The number of effects requested is too large. The number of effects which can be estimated is at most %.0f.	The command will therefore try to estimate %0.f effect(s).", l_XX, l_XX))
        effects <- l_XX
    }

    if (placebo != 0) {
        if (l_placebo_XX < placebo & effects >= placebo) {
            message(sprintf("The number of placebos which can be estimated is at most %.0f. The command will therefore try to estimate %.0f placebo(s).", l_placebo_XX, l_placebo_XX))
            placebo <- l_placebo_XX
        }
        if (effects < placebo) {
            message(sprintf("The number of placebo requested cannot be larger than the number of effects requested. The command cannot compute more than %.0f placebo(s).", effects))
            placebo <- min(l_placebo_XX, effects)
        }
    }

    df <- df[order(df$group_XX, df$time_XX), ]
    
    ## Replace treatment symmetrically for the pre-treatment periods

    if (placebo != 0) {
        if (isTRUE(trends_lin)) {
            for (i in 2:(placebo+1)) {
                df$D_XX <- ifelse(df$time_XX == df$F_g_XX - 1 - i,  lead(df$D_XX, 2*i-1), df$D_XX)
            }
        } else {
            for (i in 1:placebo) {
                df$D_XX <- ifelse(df$time_XX == df$F_g_XX - 1 - i,  lead(df$D_XX, 2*i), df$D_XX)
            }
        }
    }

    if (isTRUE(dynamic)) {
        df$post_XX <- df$time_XX >= df$F_g_XX
        df <- df %>% group_by(.data$group_XX, .data$post_XX) %>% mutate(cumulative_XX = cumsum(.data$D_XX)) %>% ungroup()
        df$cumulative_XX <- ifelse(df$post_XX, df$cumulative_XX, 0)

        if (placebo != 0) {
            if (isTRUE(trends_lin)) {
                for (i in 2:(placebo+1)) {
                    df$cumulative_XX <- ifelse(df$time_XX == df$F_g_XX - 1 - i, lead(df$cumulative_XX, 2*i-1), df$cumulative_XX)
                }
            } else {
                for (i in 1:(placebo)) {
                    df$cumulative_XX <- ifelse(df$time_XX == df$F_g_XX - 1 - i, lead(df$cumulative_XX, 2*i), df$cumulative_XX)
                }
            }
        }
        df$post_XX <- NULL
    }

    resmat <- matrix(NA, nrow = effects + placebo, ncol = 10)
    if (isTRUE(yatchew)) {
        y_resmat <- matrix(NA, nrow = effects + placebo, ncol = 5)
        colnames(y_resmat) <- c("\U03C3\U00B2_lin", "\U03C3\U00B2_diff", "T_hr", "p-value", "N")    
    }

    rown<- sapply(1:effects, function(x) paste0("Effect_", x))
    coln <- c("Estimate", "SE", "LB.CI", "UB.CI", "N", "BW", "N.BW", "T", "p.val", "ID")
    for (i in 1:effects) {
        df[[paste0("Effect_", i)]] <- ifelse(df$F_g_XX+(i-1)==df$time_XX, df$Y_XX - lag(df$Y_XX, i), NA)
        
        if (isTRUE(trends_lin)) {
            df[[paste0("Effect_", i)]] <-  df[[paste0("Effect_", i)]] - i*df$lin_trend_XX
        }
        res <- did_had_est(df = df, Y_XX = paste0("Effect_", i), D_XX = "D_XX", group_XX = "group_XX", level = level, kernel = kernel, bw_method =  bw_method, yatchew = yatchew, dynamic = dynamic)
        resmat[i, 1] <- res$beta_qs_XX
        resmat[i, 2] <- res$se_naive_XX
        resmat[i, 3] <- res$low_XX
        resmat[i, 4] <- res$up_XX
        resmat[i, 5] <- res$G_XX
        resmat[i, 6] <- res$h_star
        resmat[i, 7] <- res$within_bw_XX
        resmat[i, 8] <- res$np_qug_test[1]
        resmat[i, 9] <- res$np_qug_test[2]
        resmat[i, 10] <- i

        if (isTRUE(yatchew)) {
            y_resmat[i, 1:5] <- as.vector(res$yt_res)
        }
    }
    if (placebo != 0) {
        for (i in 1:placebo) {

            if (isTRUE(trends_lin)) {
                df[[paste0("Placebo_", i)]] <- ifelse(df$F_g_XX-(i+2) == df$time_XX, df$Y_XX - lead(df$Y_XX, i), NA)
                df[[paste0("Placebo_", i)]] <- df[[paste0("Placebo_", i)]] + i * df$lin_trend_XX
            } else {
                df[[paste0("Placebo_", i)]] <- ifelse(df$F_g_XX-(i+1) == df$time_XX, df$Y_XX - lead(df$Y_XX, i), NA)
            }
             
            res <- did_had_est(df = df, Y_XX = paste0("Placebo_", i), D_XX = "D_XX", group_XX = "group_XX", level = level, kernel = kernel, bw_method = bw_method, yatchew = yatchew, placebo = TRUE, dynamic = dynamic)
            resmat[effects+i, 1] <- res$beta_qs_XX
            resmat[effects+i, 2] <- res$se_naive_XX
            resmat[effects+i, 3] <- res$low_XX
            resmat[effects+i, 4] <- res$up_XX
            resmat[effects+i, 5] <- res$G_XX
            resmat[effects+i, 6] <- res$h_star
            resmat[effects+i, 7] <- res$within_bw_XX
            resmat[effects+i, 10] <- -i

            if (isTRUE(yatchew)) {
                y_resmat[effects+i, 1:5] <- as.vector(res$yt_res)
            }
        }
        rown <- c(rown, sapply(1:placebo, function(x) paste0("Placebo_", x)))
    }

    colnames(resmat) <- coln
    rownames(resmat) <- rown
    out <- list(resmat = resmat, res.effects = effects, res.placebo = placebo, n_stayers = n_stayers)

    if (isTRUE(yatchew)) {
        rownames(y_resmat) <- rown
        out <- append(out, list(y_resmat))
        names(out)[length(names(out))] <- "yatchew_test"
    }
    return(out)
}