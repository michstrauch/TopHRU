#' Plot results of HRU analysis
#'
#' @param hru_analysis list structure that is the result of the function
#'   topHRU()
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
plot_pareto <- function(hru_analysis, aREA_thrs = NULL, HRU_thrs = NULL){
  hru_analysis$result_all$Threshold <-
    factor(rep(2, nrow(hru_analysis$result_all)),
           levels = 1:2, labels = c("above", "below"))

  if(!is.null(aREA_thrs)){
    hru_analysis$result_all$Threshold[
      hru_analysis$result_all$aREA > aREA_thrs] <- "above"
  }
  if(!is.null(HRU_thrs)){
    hru_analysis$result_all$Threshold[
      hru_analysis$result_all$n_HRU > HRU_thrs] <- "above"
  }
  if(any(hru_analysis$result_all$Threshold == "above")){
    hru_analysis$result_all$Legend <- paste(
      hru_analysis$result_all$Pareto_front,
      hru_analysis$result_all$Threshold, sep = ", ") %>%
      ordered(., levels = c("dominated, below",
                            "non dominated, below",
                            "dominated, above",
                            "non dominated, above"))
  } else {
    hru_analysis$result_all$Legend <-
      hru_analysis$result_all$Pareto_front %>%
      ordered(., levels = c("dominated", "non dominated"))
  }

  pareto_ggplot <- ggplot() +
    geom_line(data = hru_analysis$result_nondominated,
              aes(x = n_HRU, y = aREA), col = "tomato3",
              lwd = 0.3, alpha = 0.5) +
    geom_point(data = hru_analysis$result_all,
               aes(x = n_HRU, y = aREA, col = Legend),
               size = 0.7) +
    theme_bw()

  if(any(hru_analysis$result_all$Threshold == "above")){
    pareto_ggplot <- pareto_ggplot +
    scale_color_manual(values = c("grey65","tomato3", "grey90", "tomato1"), drop = FALSE)
  } else {
    pareto_ggplot <- pareto_ggplot +
      scale_color_manual(values = c("grey65", "tomato3"), drop = FALSE)
  }
  if(!is.null(HRU_thrs)){
    pareto_ggplot <- pareto_ggplot +
      geom_vline(aes(xintercept = HRU_thrs), linetype ="dashed", size = 0.3)
  }
  if(!is.null(aREA_thrs)){
    pareto_ggplot <- pareto_ggplot +
      geom_hline(aes(yintercept = aREA_thrs), linetype ="dashed", size = 0.3)
  }


  pareto_plotly <- plotly_build(pareto_ggplot)

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
      thrs_comb_txt <- hru_analysis$result_all %>%
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
  pareto_plotly
}
