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
  hru_analysis$result_all$Threshold <- factor(rep(2, nrow(hru_analysis$result_all)),
                                              levels = 1:2, labels = c("above", "below"))
  point_aes <- aes_string(x = "n_HRU", y = "aREA", col = "Pareto_front")
  if(!is.null(aREA_thrs)){
    hru_analysis$result_all$Threshold[hru_analysis$result_all$aREA
                                      > aREA_thrs] <- "above"
    point_aes <- aes_string(x = "n_HRU", y = "aREA",
                            col = "Pareto_front",
                            alpha = "Threshold")
  }
  if(!is.null(HRU_thrs)){
    hru_analysis$result_all$Threshold[hru_analysis$result_all$n_HRU
                                      > HRU_thrs] <- "above"
    point_aes <- aes_string(x = "n_HRU", y = "aREA",
                            col = "Pareto_front",
                            alpha = "Threshold")
  }

  pareto_ggplot <- ggplot() +
    geom_line(data = hru_analysis$result_nondominated,
              aes(x = n_HRU, y = aREA), col = "tomato3",
              lwd = 0.3, alpha = 0.5) +
    geom_point(data = hru_analysis$result_all, point_aes,
               size = 0.7) +
    theme_bw() +
    scale_color_manual(values = c("grey70", "tomato3")) +
    scale_alpha_discrete(drop = FALSE)

  if(!is.null(HRU_thrs)){
    pareto_ggplot <- pareto_ggplot +
      geom_vline(aes(xintercept = HRU_thrs), linetype ="dashed", size = 0.3)
  }
  if(!is.null(aREA_thrs)){
    pareto_ggplot <- pareto_ggplot +
      geom_hline(aes(yintercept = aREA_thrs), linetype ="dashed", size = 0.3)
  }


  pareto_plotly <- plotly_build(pareto_ggplot)

  pareto_lbl <- c("dominated", "non dominated")

  for (i in 1:length(pareto_plotly$x$data)){
    plotly_lbl <- pareto_plotly$x$data[[i]]$text[1] %>%
      strsplit("<br>|:") %>%
      unlist %>%
      gsub("^\\s+|\\s+$", "", .)

    pareto_filt <- pareto_lbl[pareto_lbl %in% plotly_lbl]
    if("above" %in% plotly_lbl){
      thrs_filt <- "above"
    } else{
      thrs_filt   <- "below"
    }

    if(length(pareto_filt) > 0){
      thrs_comb_txt <- hru_analysis$result_all %>%
        filter(Threshold == thrs_filt,
               Pareto_front == pareto_filt) %>%
        .$thrs_comb

      label_txt <- pareto_plotly$x$data[[i]]$text
      label_txt <- sub("Pareto_front: dominated","",label_txt)
      label_txt <- sub("Pareto_front: non dominated","",label_txt)
      label_txt <- sub("Threshold: below","",label_txt)
      label_txt <- sub("Threshold: above","",label_txt)
      label_txt <- sub("<br><br>","<br>",label_txt)
      label_txt <- sub("<br><br>","<br>",label_txt)
      label_txt <- paste0("thrs_comb: ",thrs_comb_txt, label_txt)
      pareto_plotly$x$data[[i]]$text <- label_txt
    }

    rm(pareto_filt)
  }
  pareto_plotly
}
