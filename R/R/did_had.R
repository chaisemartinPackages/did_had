#' Outer function for DIDHetAdoption package
#' @md
#' @description Estimation of DiD estimators in Heterogeneous adoption designs (de Chaisemartin, D'Haultfoeuille & Gurgand, 2024). 
#' @param df A data.frame object
#' @param outcome Outcome variable
#' @param group Group Variable
#' @param time Time variable
#' @param treatment Treatment variable
#' @param effects effects
#' @param placebo placebo
#' @param level level
#' @param kernel kernel
#' @param graph_off graph_off
#' @param no_data no_data
#' @examples
#' A <- 0; print(A)
#' @export
did_had <- function(
    df,
    outcome,
    group,
    time,
    treatment,
    effects = 1,
    placebo = 0,
    level = 5,
    kernel = "uniform",
    graph_off = FALSE,
    no_data = FALSE
) {

    params <- as.list(match.call())[-1]
    args <- list()  
    for (v in names(formals(did_had))) {
        if (!is.null(get(v))) {
            if (v == "df") {
                if (!inherits(get(v), "data.frame")) {
                    stop("Syntax error in df option. Data.frame required.")
                }
            }
            else if (v %in% c("outcome", "treatment", "group", "time")) {
                if (!(inherits(get(v), "character") & length(get(v)) == 1)) {
                    stop(sprintf("Syntax error in %s option. Single string required required.", v))
                }
            }
            else if (v %in% c("level")) {
                if (!(inherits(get(v), "numeric") & get(v) %% 1 == 0)) {
                    stop(sprintf("Syntax error in %s option. Numeric integer required required.", v))
                }
            }
        }
        if (v != "df") {
            args[[v]] <- get(v)
        }
    }
    args$df <- params$df
    params <- NULL

    obj <- list(args)
    results <- did_het_adoption_main(df = df, outcome = outcome, group = group, time = time, treatment = treatment, effects = effects, placebo = placebo, level = level)
    obj <- append(obj, list(results$mat))
    obj <- append(obj, list(results$h_scalars))
    names(obj) <- c("args", "results", "h_scalars")
    if (isFALSE(no_data)) {
        obj <- append(obj, list(results$data))
        names(obj)[length(names(obj))] <- "data"
    }
    
    class(obj) <- "did_had"
    return(obj)
}
