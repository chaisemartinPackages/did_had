#' Outer function for DIDHetAdoption package
#' @importFrom haven read_dta
#' @md
#' @description Estimates the effect of a treatment on an outcome in a heterogeneous adoption design with no stayers but some quasi stayers (de Chaisemartin and D'Haultfoeuille, 2024).
#' @param df (data.frame) A data.frame object
#' @param outcome (character) Outcome variable
#' @param group (character) Group Variable
#' @param time (character) Time variable
#' @param treatment (character) Treatment variable
#' @param effects (positive numeric) allows you to specify the number of effects \code{did_had()} tries to estimate. Effect \eqn{\ell} is the treatment's effect at period \eqn{F-1+\ell}, namely \eqn{\ell} periods after adoption. By default, the command estimates only 1 effect and in case you specified more effects than your data allows to estimate the number of effects is automatically adjusted to the maximum. 
#' @param placebo (nonnegative numeric) allows you to specify the number of placebo estimates \code{did_had()} tries to compute. Those placebos are constructed symmetrically to the estimators of the actual effects, except that the outcome evolution from \eqn{F-1} to \eqn{F-1+\ell} in the actual estimator is replaced by the outcome evolution from \eqn{F-1} to \eqn{F-1-\ell} in the placebo.  
#' @param level (positive numeric) allows you to specify (1-the level) of the confidence intervals shown by the command. By default this level is set to 0.05, thus yielding 95% level confidence intervals.
#' @param kernel (character in "tri", "epa", "uni" or "gau") allows you to specify the kernel function used by \code{lprobust()}. Possible choices are triangular, epanechnikov, uniform and gaussian. By default, the program uses a uniform kernel.
#' @param yatchew (logical) yatchew yields the result from a non-parametric test that the conditional expectation of the \eqn{F-1} to \eqn{F-1+\ell} outcome evolution given the treatment at \eqn{F-1+\ell} is linear (Yatchew, 1997). This test is implemented using the heteroskedasticity-robust test statistic proposed in Section 3 of de Chaisemartin and D'Haultfoeuille (2024) and it is performed for all the dynamic effects and placebos computed by \code{did_had}. This option requires the YatchewTest package, which is currently available on CRAN.
#' @param graph_off (logical) by default, \code{did_had()} outputs an event-study graph with the effect and placebo estimates and their confidence intervals. When specifying \code{graph_off = TRUE}, the graph is suppressed.
#' @section Overview:
#' \code{did_had()} estimates the effect of a treatment on an outcome in a heterogeneous adoption design (HAD) with no stayers but some quasi stayers. HADs are designs where all groups are untreated in the first period, and then some groups receive a strictly positive treatment dose at a period \eqn{F}, which has to be the same for all treated groups (with variation in treatment timing, the \code{did_multiplegt_dyn()} package may be used). Therefore, there is variation in treatment intensity, but no variation in treatment timing. HADs without stayers are designs where all groups receive a strictly positive treatment dose at period \eqn{F}: no group remains untreated. Then, one cannot use untreated units to recover the counterfactual outcome evolution that treated groups would have experienced from before to after \eqn{F}, without treatment. 
#' 
#' To circumvent this, \code{did_had()} implements the estimator from de Chaisemartin and D'Haultfoeuille (2024) which uses so-called "quasi stayers" as the control group. Quasi stayers are groups that receive a "small enough" treatment dose at F to be regarded as "as good as untreated". Therefore, \code{did_had()} can only be used if there are groups with a treatment dose "close to zero". Formally, the density of groups' period-two treatment dose needs to be strictly positive at zero, something that can be assessed by plotting a kernel density estimate of that density. 
#' 
#' The command makes use of the \code{lprobust()} command by Calonico, Cattaneo and Farrell (2019) to determine an optimal bandwidth, i.e. a treatment dose below which groups can be considered as quasi stayers. To estimate the treatment's effect, the command starts by computing the difference between the change in outcome of all groups and the intercept in a local linear regression of the outcome change on the treatment dose among quasi-stayers. Then, that difference is scaled by groups' average treatment dose at period two. Standard errors and confidence intervals are also computed leveraging \code{lprobust()}. We recommend that users of {cmd:did_had} cite de Chaisemartin and D'Haultfoeuille (2024), Calonico, Cattaneo and Farrell (2019), and Calonico, Cattaneo and Farrell (2018). 
#' 
#' @section Contacts:
#' 
#' Github repository: [chaisemartinPackages/did_had](https://github.com/chaisemartinPackages/did_had)
#' 
#' Mail: [chaisemartin.packages@gmail.com](mailto:chaisemartin.packages@gmail.com)
#' 
#' @section References:
#' 
#' de Chaisemartin, C and D'Haultfoeuille, X (2024). [Two-way Fixed Effects and Difference-in-Difference Estimators in Heterogeneous Adoption Designs](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4284811)
#' 
#' Calonico, S., M. D. Cattaneo, and M. H. Farrell. (2019). [nprobust: Nonparametric Kernel-Based Estimation and Robust Bias-Corrected Inference](https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2019_JSS.pdf).
#' 
#' Calonico, S., M. D. Cattaneo, and M. H. Farrell. (2018). [On the Effect of Bias Estimation on Coverage Accuracy in Nonparametric Inference](https://nppackages.github.io/references/Calonico-Cattaneo-Farrell_2018_JASA.pdf).
#' 
#' Yatchew, A. (1997). [An elementary estimator of the partial linear model](https://www.sciencedirect.com/science/article/pii/S0165176597002188).
#' 
#' @examples
#' # The sample data for this example can be downloaded by running:
#' repo <-"https://raw.githubusercontent.com/chaisemartinPackages/did_had/" 
#' data <- haven::read_dta(paste0(repo,"main/tutorial_data.dta"))
#' 
#' # Estimating the effects over five periods and placebos for four pre-treatment periods, 
#' # suppressing the graph and with a triagular kernel:
#' 
#' summary(did_had(df = data, 
#'                 outcome = "y",
#'                 group = "g",
#'                 time = "t",
#'                 treatment = "d",
#'                 effects = 5,
#'                 placebo = 4,
#'                 kernel = "tri",
#'                 graph_off = TRUE))
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
    kernel = "uni",
    yatchew = FALSE,
    graph_off = FALSE
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
    results <- did_het_adoption_main(df = df, outcome = outcome, group = group, time = time, 
        treatment = treatment, effects = effects, placebo = placebo, level = level, kernel = kernel, yatchew = yatchew)
    obj <- append(obj, list(results))
    obj <- append(obj, list(did_had_graph(results)))
    names(obj) <- c("args", "results", "plot")

    if (isFALSE(graph_off)) {
        print(obj$plot)
    }
    
    class(obj) <- "did_had"
    return(obj)
}
