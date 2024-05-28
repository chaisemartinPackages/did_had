#' Internal function of did_het_adoption for cross validation
#' @param reg regression formula
#' @param D D
#' @param Y Y
#' @param df df
#' @param ref ref
#' @param N N
#' @importFrom MASS ginv
#' @importFrom stats quantile lm predict
#' @noRd
cross_validation <- function(reg, D, Y, df, ref = 0.5, lev = 1, N = 20) {
    h <- 0; cv <- Inf; temp_cv <- Inf;
    min <- ref - lev/2; max <- ref + lev/2; 
    min <- if (min < 0) 0 else min
    max <- if (max > 1) 1 else max
    grid <- seq(min, max, length.out = N + 1)[2:N]
    for (p in grid) {
        df$partition_XX <- as.numeric(df[[D]] < quantile(df[[D]], p, type = 2))
        if (sum(df$partition_XX, na.rm = TRUE) == 0) {
            df$partition_XX <- NULL
            next
        } else {
            df_base <- subset(df, df$partition_XX == 1)
            df$partition_XX <- NULL
        }
        model <- lm(reg, data = df_base)
        df_base$e_i <- predict(model, df_base, na.rm = TRUE)
        df_base$e_i_sq <-  (as.numeric(df_base[[Y]]) - df_base$e_i)^2

        X_lab <- attributes(model$terms)$term.labels
        X <- as.matrix(df_base[X_lab], ncol = length(X_lab))
        df_base$H_sq <- (1 - diag(X %*% ginv(t(X) %*% X) %*% t(X)))^2
        temp_cv <- mean(df_base$e_i_sq/df_base$H_sq, na.rm = TRUE)
        if (!is.nan(temp_cv)) {
            if (temp_cv < cv) {
                cv <- temp_cv
                h <- p
            }
        }
        df_base <- NULL
    }
    return(h)
}