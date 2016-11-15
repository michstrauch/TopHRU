#' Plot results of HRU analysis
#'
#' @param hru_analysis list structure that is the result of the function
#'   topHRU()
#' @param interactive Logic: \code{TRUE}: Output will be an
#'   interactive plotly object helpful in selecting adequate threshold
#'   combinations, or \code{FALSE} for a static ggplot object useful for
#'   publication reasons.
#' @import ggplot2
#' @importFrom plotly plotly_build
#'
#'
#' @return Returns an interactive or a static plot object of the aREA
#'   measure over the resulting number of HRUs of HRUs for the respective
#'   threshold combinations.
#' @export
#'
#' @examples
plot_pareto <- function(hru_analysis, interactive = TRUE){
  if(interactive){
    pareto_ggplot <- ggplot() +
      geom_line(data = hru_analysis$result_nondominated,
                aes(x = n_HRU, y = aREA), col = "tomato3",
                lwd = 0.3, alpha = 0.5) +
      geom_point(data = hru_analysis$result_all,
                 aes(x = n_HRU, y = aREA, col = Pareto_front),
                 size = 0.7) +
      theme_bw() +
      scale_color_manual(values = c("grey70", "tomato3"))
    pareto_plotly <- plotly_build(pareto_ggplot)
    label_dom <- pareto_plotly$x$data[[2]]$text
    label_dom <- sub("<br>Pareto_front: dominated","",label_dom)
    label_dom <- paste0("thrs_comb: ",
                        hru_analysis$result_all$thrs_comb[
                          hru_analysis$result_all$Pareto_front == "dominated"],
                        "<br>",
                        label_dom)
    label_nondom <- pareto_plotly$x$data[[3]]$text
    label_nondom <- sub("<br>Pareto_front: non dominated","",label_nondom)
    label_nondom <- paste0("thrs_comb: ",
                           hru_analysis$result_all$thrs_comb[
                             hru_analysis$result_all$Pareto_front == "non dominated"],
                           "<br>",
                           label_nondom)
    pareto_plotly$x$data[[1]]$text <- ""
    pareto_plotly$x$data[[2]]$text <- label_dom
    pareto_plotly$x$data[[3]]$text <- label_nondom
    pareto_plotly
  } else {
    pareto_ggplot <- ggplot() +
      geom_line(data = hru_analysis$result_nondominated,
                aes(x = n_HRU, y = aREA), col = "tomato3",
                alpha = 0.5) +
      geom_point(data = hru_analysis$result_all,
                 aes(x = n_HRU, y = aREA, col = Pareto_front)) +
      theme_bw() +
      scale_color_manual(values = c("grey70", "tomato3"))
    pareto_ggplot
  }
}
