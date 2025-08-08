# Internal function to produce a graph
#' @param obj A did_had obj$results object
#' @import ggplot2
#' @importFrom dplyr %>% group_by mutate cur_group_id ungroup lag lead
#' @returns A ggplot object with the event study graph.
#' @noRd

did_had_graph <- function(obj) {
    gr_mat <- as.data.frame(rbind(obj$resmat, rep(0,10)))
    gr_mat <- gr_mat[order(gr_mat$ID), ]
    gr_obj <- ggplot(gr_mat,aes(x = .data$ID, y = .data$Estimate, group = 1)) + 
    geom_line(colour = "blue") + 
    geom_errorbar(data = ~dplyr::filter(.x, gr_mat$Estimate != 0),
        aes(ymin = .data$LB.CI, ymax = .data$UB.CI), position=position_dodge(0.05), width = 0.2, colour = "red")  + 
    geom_point(colour = "blue") + 
    ggtitle("Estimates from DIDHAD") + 
    xlab("Relative time to treatment change") + ylab("Effect") +
    scale_x_continuous(breaks=seq(-obj$res.placebo,obj$res.effects,1)) +
    theme(plot.title = element_text(hjust = 0.5))
    return(gr_obj)
}