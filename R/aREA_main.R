#' topHRU - threshold optimization for HRUs in SWAT
#'
#' @param hru_data data.frame with the unaggregated information for the HRUs
#' @param luse_thrs Land use threshold vector with min, max and stepwidth.
#' @param soil_thrs Soil threshold vector with min, max and stepwidth.
#' @param slp_thrs Slope threshold vector with min, max and stepwidth.
#' @param thrs_type Threshold type. All threshold vectors must be given as
#'   percentages when set as 'P' or area in ha when set to 'A'.
#' @param weight Weights for land use, soil and slope for the calculation of
#'   aREA. Any given numbers will be normalized.
#'
#' @return \code{.$result_all}: data.frame with the complete results of the
#'   analysis.\cr \code{.$result_nondominated}: Results for the non dominated threshold
#'   combinations.\cr \code{.$pareto_plot}: Interactive plot of the dominated and non
#'   dominated threshold combination.
#' @export
#'
#' @examples
#'
#' hru_data <- hru_data
#' hru_analysis <- topHRU(hru_data)
#' hru_analysis$pareto_plot

topHRU <- function(hru_data, luse_thrs = c(0,20,5), soil_thrs = c(0,20,5),
                   slp_thrs = c(0,20,5), thrs_type = c("P", "A"),
                   weight = c(1,1,1)) {

  if(any( c(luse_thrs, soil_thrs, slp_thrs) < 0)){
    stop("Any of the input threshold values is negative.
          All values must be positive!")
  }
  thrs_type <- thrs_type[1]
  switch(thrs_type,
         "A" = {thrs_fact <- 1},
         "P" = {thrs_fact <- 1/100},
         stop("Invalid threshold type. Must be either 'A', or 'P'!"))

  if (length(unique(hru_data$LANDUSE)) == 1) {
    warning("Only one unique land use class found. Input land use thresholds
            will be ignored!")
    luse_thrs <- c(0,0,1)
  }

  if (length(unique(hru_data$SOIL)) == 1) {
    warning("Only one unique soil class found. Input soil thresholds will be
            ignored!")
    soil_thrs <- c(0,0,1)
  }

  if (length(unique(hru_data$SLP)) == 1) {
    warning("Only one unique slope class found. Input slope thresholds will be
            ignored!")
    slp_thrs <- c(0,0,1)
  }

  # Generate threshold combinations
  luse_seq <- seq(thrs_fact*luse_thrs[1],
                  thrs_fact*luse_thrs[2],
                  thrs_fact*luse_thrs[3])
  soil_seq <- seq(thrs_fact*soil_thrs[1],
                  thrs_fact*soil_thrs[2],
                  thrs_fact*soil_thrs[3])
  slp_seq  <- seq(thrs_fact*slp_thrs[1],
                  thrs_fact*slp_thrs[2],
                  thrs_fact*slp_thrs[3])

  thrs_comb <- expand.grid(luse_seq, soil_seq, slp_seq)
  colnames(thrs_comb) <- c("LUSE_thrs", "SOIL_thrs", "SLOPE_thr")


  # Read out all existing categories for subbasin, landuse, soil and slope
  sub_id  <- sort(unique(hru_data$SUBBASIN))
  luse_id <- sort(unique(hru_data$LANDUSE))
  soil_id <- sort(unique(hru_data$SOIL))
  slp_id  <- sort(unique(hru_data$SLP))

  # Generate data structures for default area distribution of each hierarchy
  # level. Default distribution is the distribution without thresholds applied,
  # not aggregated ("tree structure")
  luse_default <- matrix(data = NA, ncol = length(sub_id),
                         nrow = length(luse_id), dimnames = list(luse_id,
                                                                 sub_id))
  soil_default <- array(data = NA, dim = c(length(soil_id), length(sub_id),
                                           length(luse_id)),
                        dimnames = list(soil_id, sub_id, luse_id))
  slp_default <- array(data = NA, dim = c(length(slp_id), length(sub_id),
                                            length(luse_id), length(soil_id)),
                       dimnames = list(slp_id, sub_id, luse_id, soil_id))

  # Calculate default area distribution of each hierachy level, set non
  # existing values from NA to 0
  luse_default <- with(hru_data, tapply(ARSLP, list(LANDUSE, SUBBASIN), sum))
  luse_default[is.na(luse_default)] <- 0

  soil_default[,,] <- with(hru_data, tapply(ARSLP, list(SOIL, SUBBASIN,
                                                        LANDUSE), sum))
  soil_default[,,][is.na(soil_default[,,])] <- 0

  slp_default[,,,] <- with(hru_data, tapply(ARSLP, list(SLP, SUBBASIN,
                                                        LANDUSE, SOIL), sum))
  slp_default[,,,][is.na(slp_default[,,,])] <- 0

  # Generate data structures for the area reference distribution of each
  # hierachy level reference distribution is the distribution without
  # thresholds applied, aggregated for each hierarchy level per sub
  luse_ref <- array(data = NA, dim = c(length(sub_id), length(luse_id),
                                       nrow(thrs_comb)),
                    dimnames = list(sub_id, luse_id, rownames(thrs_comb)))
  soil_ref <- array(data = NA, dim = c(length(sub_id), length(soil_id),
                                       nrow(thrs_comb)),
                    dimnames = list(sub_id, soil_id, rownames(thrs_comb)))
  slp_ref <- array(data = NA, dim = c(length(sub_id), length(slp_id),
                                        nrow(thrs_comb)),
                   dimnames = list(sub_id, slp_id, rownames(thrs_comb)))

  # Generate data structures for computed area distribution of each hierachy
  # level Computed distribution is the distribution with thresholds applied,
  # aggregated for each hierarchy level per sub
  luse_cal <- array(data = NA, dim = c(length(sub_id), length(luse_id),
                                     nrow(thrs_comb)),
                  dimnames = list(sub_id, luse_id, rownames(thrs_comb)))
  soil_cal <- array(data = NA, dim = c(length(sub_id), length(soil_id),
                                       nrow(thrs_comb)),
                    dimnames = list(sub_id, soil_id, rownames(thrs_comb)))
  slp_cal <- array(data = NA, dim = c(length(sub_id), length(slp_id),
                                        nrow(thrs_comb)),
                     dimnames = list(sub_id, slp_id, rownames(thrs_comb)))

  # Generate data structures for factors that are applied according default
  # distribution.
  # Factors are generated by applying thresholds to default distribution
  # if a factor = 0, then the element is erased,
  # if a factor = 1, then all elements remain unchanged,
  # if a factor > 1, then the element is reapportionated by the factor due to
  #                  loss of other elements
  luse_fac <- array(data = NA, dim = c(length(luse_id), length(sub_id),
                                       length(luse_seq)),
                    dimnames = list(luse_id, sub_id, luse_seq))
  soil_fac <- array(data = NA, dim = c(length(soil_id), length(sub_id),
                                       length(luse_id), length(soil_seq)),
                    dimnames = list(soil_id, sub_id, luse_id, soil_seq))
  slp_fac <- array(data = NA, dim = c(length(slp_id), length(sub_id),
                                      length(luse_id),length(soil_id),
                                      length(slp_seq)),
                    dimnames = list(slp_id, sub_id, luse_id, soil_id, slp_seq))

  # Generate data structure for the residuals between the reference and
  # computed distribution
  luse_err <- array(data = NA, dim = c(length(sub_id), length(luse_id),
                                       nrow(thrs_comb)),
                    dimnames = list(sub_id, luse_id, rownames(thrs_comb)))
  soil_err <- array(data = NA, dim = c(length(sub_id), length(soil_id),
                                       nrow(thrs_comb)),
                    dimnames = list(sub_id, soil_id, rownames(thrs_comb)))
  slp_err <- array(data = NA, dim = c(length(sub_id), length(slp_id),
                                      nrow(thrs_comb)),
                   dimnames = list(sub_id, slp_id, rownames(thrs_comb)))

  # Generate data structure for the result of the analysis
  result <- data.frame(matrix(data = NA, ncol = 6, nrow = nrow(thrs_comb),
                              dimnames = list(NULL, c("thrs_comb",
                                                      "n_HRU",
                                                      "luse_res",
                                                      "soil_res",
                                                      "slp_res",
                                                      "aREA"))))

  ## CALCULATION
  # The part where the actual calculation takes places
  # Calculation of factors for each hierarchical level,
  # for LANDUSE the factors depend on landuse, subbasin and threshold
  # (in that order in array dimensions)
  # for SOIL the factors depend on soil, subbasin, landuse and threshold
  # for SLOPE the factors depend on slope, subbasin, landuse, soil and threshold
  for (i in seq(along = luse_seq)){
    luse_fac[,,i] <- apply(luse_default, 2, reapp_fac,
                           thrs = luse_seq[i], thrs_type = thrs_type)
    }

  for (i in seq(along = soil_seq)){
    soil_fac[,,,i] <- apply(soil_default, c(2, 3), reapp_fac,
                            thrs = soil_seq[i], thrs_type = thrs_type)
  }

  for (i in seq(along = slp_seq)){
    slp_fac[,,,,i] <- apply(slp_default, c(2, 3, 4), reapp_fac,
                            thrs = slp_seq[i], thrs_type = thrs_type)
  }

  # Calculation of area distributions for each hierarchy level depending on
  # threshold combination
  # AREA for each default HRU is multiplied by the factors for each level
  # Factors are selected from factor data structure, e.g. SLOPE.fac
  # Using the columns LANDUSE, SOIL, SLOPE in hru_data and the according
  # threshold as indices
  # the so "updated AREA" is then aggregated according to each hierarchy level
  # The number of resulting HRU is calculated by counting all cells of "updated
  # AREA" that are > 0
  for (j in 1:nrow(thrs_comb)){
    area_mod <- with(hru_data, ARSLP *
                       luse_fac[cbind(LANDUSE, SUBBASIN,
                                      which(luse_seq == thrs_comb[j, 1]))] *
                       soil_fac[cbind(SOIL, SUBBASIN, LANDUSE,
                                      which(soil_seq == thrs_comb[j, 2]))] *
                       slp_fac[cbind(SLP, SUBBASIN, LANDUSE, SOIL,
                                     which(slp_seq == thrs_comb[j, 3]))])
    luse_cal[,,j] <- with(hru_data, tapply(area_mod,
                                           list(SUBBASIN, LANDUSE), sum))
    soil_cal[,,j] <- with(hru_data, tapply(area_mod,
                                           list(SUBBASIN, SOIL), sum))
    slp_cal[,,j]  <- with(hru_data, tapply(area_mod,
                                           list(SUBBASIN, SLP), sum))

    result[j,2] <- with(hru_data, sum(area_mod > 0))
  }

  # Set non existing values from NA to 0
  luse_cal[,,][is.na(luse_cal[,,])] <- 0
  soil_cal[,,][is.na(soil_cal[,,])] <- 0
  slp_cal[,,] [is.na(slp_cal[,,])]  <- 0

  # Calculate reference area distribution of each hierachy level, extend
  # format of matrix by the number of threshold combinations to match the
  # format of data structure for calculated values set non existing values
  # from NA to 0
  luse_ref <- array(rep(with(hru_data,
                             tapply(ARSLP, list(SUBBASIN, LANDUSE), sum)),
                        times = nrow(thrs_comb)),
                    dim = c(length(sub_id),length(luse_id),nrow(thrs_comb)))
  luse_ref[is.na(luse_ref)] <- 0
  soil_ref <- array(rep(with(hru_data,
                             tapply(ARSLP, list(SUBBASIN, SOIL), sum)),
                        times = nrow(thrs_comb)),
                    dim = c(length(sub_id),length(soil_id),nrow(thrs_comb)))
  soil_ref[is.na(soil_ref)] <- 0
  slp_ref <- array(rep(with(hru_data,
                            tapply(ARSLP, list(SUBBASIN, SLP), sum)),
                       times = nrow(thrs_comb)),
                     dim = c(length(sub_id),length(slp_id),nrow(thrs_comb)))
  slp_ref[is.na(slp_ref)] <- 0

  # Calculate residuals between computed and reference values and divide by two
  luse_err <- (luse_cal - luse_ref)
  soil_err <- (soil_cal - soil_ref)
  slp_err  <- (slp_cal  - slp_ref)

  # Create results table showing thr_set, number of calculated HRU and error
  # measure
  # For percentage method convert fraction between 0 and 1 to percentage

  # Combine single thresholds to single character
  thrs_comb <- thrs_comb / thrs_fact

  result[,1] <- paste(thrs_comb$LUSE_thrs,
                      thrs_comb$SOIL_thrs,
                      thrs_comb$SLOPE_thr,
                      sep = "_")

  result[,3] <- apply(luse_err, 3, aREA) / with(hru_data, sum(ARSLP))
  result[,4] <- apply(soil_err, 3, aREA) / with(hru_data, sum(ARSLP))
  result[,5] <- apply(slp_err,  3, aREA) / with(hru_data, sum(ARSLP))

  # Apply error measure
  weight <- 3*weight/sum(weight)
  n_crit <- 3 - sum(c(max(luse_seq) == 0,
                      max(soil_seq) == 0,
                      max(slp_seq) == 0))

  result[,6] <- apply(abind::abind(weight[1]*luse_err,
                                   weight[2]*soil_err,
                                   weight[3]*slp_err, along = 2), 3, aREA) /
    (n_crit * with(hru_data, sum(ARSLP)))

  dom_set <- emoa::is_dominated(as.matrix(t(result[,c(2,6)])))
  result_nondom <- result[!dom_set,]

  result$Pareto_front <- NA
  result$Pareto_front[!dom_set] <- "non dominated"
  result$Pareto_front[dom_set] <- "dominated"

  pareto_ggplot <- ggplot2::ggplot(data = result,
                                   ggplot2::aes(x = n_HRU,
                                                y = aREA,
                                                col = Pareto_front)) +
    ggplot2::geom_point(size = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(values = c("grey70", "tomato3"))

  pareto_plotly <- plotly::plotly_build(pareto_ggplot)
  label_dom <- pareto_plotly$data[[1]]$text
  label_dom <- sub("<br>Pareto_front: dominated","",label_dom)
  label_dom <- paste0("thrs_comb: ", result$thrs_comb[dom_set], "<br>",
                      label_dom)
  label_nondom <- pareto_plotly$data[[2]]$text
  label_nondom <- sub("<br>Pareto_front: non dominated","",label_nondom)
  label_nondom <- paste0("thrs_comb: ", result$thrs_comb[!dom_set], "<br>",
                      label_nondom)

  pareto_plotly$data[[1]]$text <- label_dom
  pareto_plotly$data[[2]]$text <- label_nondom

  out_list <- list(result_all = result,
                   result_nondominated = result_nondom,
                   pareto_plot = pareto_plotly)

  return(out_list)
}
