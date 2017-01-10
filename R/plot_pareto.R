#' Plot results of HRU analysis
#'
#' @param eval_hru list structure that is the result of the function
#'   evaluate_hru()
#' @param area_thrs adds a horizontal separation in the visualization at respective threshold value
#' @param hru_thrs adds a vertical separation in the visualization at respective threshold value
#' @param interactive Logic: \code{TRUE}: Output will be an
#'   interactive plotly object helpful in selecting adequate threshold
#'   combinations, or \code{FALSE} for a static ggplot object useful for
#'   publication reasons.
#' @import dplyr
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
#' hru_eval <- evaluate_hru(hru_demo)
#' plot_pareto(hru_eval, area_thrs = 0.2, interactive = TRUE)
#' plot_pareto(hru_eval, hru_thrs = 1500, interactive = FALSE)

plot_pareto <- function(eval_hru, area_thrs = NULL, hru_thrs = NULL, interactive = TRUE){
  eval_hru$result_all$Threshold <-
    factor(rep(2, nrow(eval_hru$result_all)),
           levels = 1:2, labels = c("above", "below"))

  if(!is.null(area_thrs)){
    eval_hru$result_all$Threshold[
      eval_hru$result_all$aREA > area_thrs] <- "above"
  }
  if(!is.null(hru_thrs)){
    eval_hru$result_all$Threshold[
      eval_hru$result_all$n_HRU > hru_thrs] <- "above"
  }
  if(any(eval_hru$result_all$Threshold == "above")){
    eval_hru$result_all$Legend <- paste(
      eval_hru$result_all$Pareto_front,
      eval_hru$result_all$Threshold, sep = ", ") %>%
      ordered(., levels = c("dominated, below",
                            "non dominated, below",
                            "dominated, above",
                            "non dominated, above"))
  } else {
    eval_hru$result_all$Legend <-
      eval_hru$result_all$Pareto_front %>%
      ordered(., levels = c("dominated", "non dominated"))
  }

  scale_fct <- ifelse(interactive,1,1.8)

  pareto_plot <- ggplot() +
    geom_line(data = eval_hru$result_nondominated,
              aes(x = n_HRU, y = aREA), col = "tomato3",
              lwd = scale_fct*0.3, alpha = scale_fct*0.5) +
    geom_point(data = eval_hru$result_all,
               aes(x = n_HRU, y = aREA, col = Legend),
               size = scale_fct*0.7) +
    theme_bw()

  if(any(eval_hru$result_all$Threshold == "above")){
    pareto_plot <- pareto_plot +
    scale_color_manual(values = c("grey65","tomato3", "grey90", "tomato1"), drop = FALSE)
  } else {
    pareto_ggplot <- pareto_plot +
      scale_color_manual(values = c("grey65", "tomato3"), drop = FALSE)
  }
  if(!is.null(hru_thrs)){
    pareto_plot <- pareto_plot +
      geom_vline(aes(xintercept = hru_thrs), linetype ="dashed", size = scale_fct*0.3)
  }
  if(!is.null(area_thrs)){
    pareto_plot <- pareto_plot +
      geom_hline(aes(yintercept = area_thrs), linetype ="dashed", size = scale_fct*0.3)
  }

  if(interactive){
    pareto_plotly <- plotly_build(pareto_plot)

    legend_lbl <- c("dominated, above", "non dominated, above",
                    "dominated, below", "non dominated, below",
                    "dominated", "non dominated")

    for (i in 1:length(pareto_plotly$x$data)){
      plotly_lbl <- pareto_plotly$x$data[[i]]$text[1] %>%
        strsplit("<br>|:") %>%
        unlist %>%
        gsub("^\\s+|\\s+$", "", .)

      legend_filt <- legend_lbl[legend_lbl %in% plotly_lbl]


      if(length(legend_filt) > 0){
        thrs_comb_txt <- eval_hru$result_all %>%
          filter(Legend == legend_filt) %>%
          .$thrs_comb

        label_txt <- pareto_plotly$x$data[[i]]$text
        for (rmv in legend_lbl){
          label_txt <- sub(paste0("<br>Legend: ", rmv),"",label_txt)
        }

        label_txt <- paste0("thrs_comb: ",thrs_comb_txt, "<br>", label_txt)
        pareto_plotly$x$data[[i]]$text <- label_txt
      }

      rm(legend_filt)
    }
    return(pareto_plotly)
  } else {
    return(pareto_plot)
  }
}
