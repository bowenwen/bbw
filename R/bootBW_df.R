################################################################################
#'
#' \code{bootBW_df} : Blocked Weighted Bootstrap using dplyr sample_n() for faster sampling and flexible analysis.
#'
#' The \code{blocked weighted bootstrap (BBW)} is an estimation technique for
#' use with data from two-stage cluster sampled surveys in which either prior
#' weighting (e.g. \code{population proportional sampling} or \code{PPS} as
#' used in \code{SMART} surveys) or posterior weighting (e.g. as used in
#' \code{RAM} and \code{S3M} surveys).
#'
#' @param x A data frame with primary sampling unit (PSU) in column named \code{psu}
#'   , survey weight (i.e. PSU population) in column named \code{pop}
#'   , and strata (i.e. region or rate bins) with whole numbers (1,2,3...) in column named \code{strata}
#'   Note: set pop all to 1 to disable weight, set strata all to 1 to disable strata
#' @param statistic A function operating on data in \code{x} (see Example)
#' @param replicates Number of bootstrap replicates
#' @return A data frame with:
#' \describe{
#' \item{}{\code{ncol} = columns_of_summary_stat + 1_trial_column)}
#' \item{}{\code{nrow} = replicates * rows_of_summary_stat}
#' \item{}{\code{names} = corresponds_to_table_X}
#' }
#' @examples
#'
#' ## stratify by region
#' villageData$strata <- villageData$region
#'
#' ## define summarise function for each bootstraped sample
#' my_summarise <- function(df, ...) {
#'   df %>%
#'     filter(!is.na(wash4)) %>%
#'     group_by(mMUAC) %>%
#'     summarise(mean_wash4 = round(mean(wash4), 0))
#' }
#'
#' ## perform bootstraping
#' bootP_df <- bootBW_df(
#'   x = indicatorsHH,
#'   sw = villageData,
#'   statistic = my_summarise,
#'   replicates = 1000
#' )
#'
#' ## analyze result of boostrapped samples
#' bootP_df %>%
#'   group_by(mMUAC) %>%
#'   summarise(
#'     min = min(mean_wash4),
#'     Q25 = quantile(mean_wash4, 0.25),
#'     avg = mean(mean_wash4),
#'     Q75 = quantile(mean_wash4, 0.75),
#'     max = max(mean_wash4)
#'   )
#'
#' # Note: dplyr style pipe with this function is not supported by design since there are two dataframe inputs.
#'
bootBW_df <- function(x, sw = NULL, summary = NULL, statistic = NULL, replicates = 1000) {
  if(is.null(sw)){
    sw <- x %>% distinct(psu, pop, strata)
  }
  #
  # multiprocessing settings for library(doParallel)
  #
  # no_cores <- detectCores() - 1
  # cl <- makeCluster(no_cores)
  # registerDoParallel(cl)

  #
  # Create data.frame for output
  #
  result_df <- data.frame()
  result_df_list <- list()
  sampled_hh_df <- data.frame()
  sampled_hh_df_list <- list()
  output <- list()
  output_list <- list()
  #
  # Resample for all replicates
  #
  # output_list <- foreach(i=icount(replicates),
  #                           .packages=c('tidyverse')) %dopar% {
  for (i in 1:replicates) {
    # Stratification
    #
    sample_strata <- function(df, ...) {
      df %>% sample_n(nrow(.), weight = pop, replace = TRUE)
    }
    psu_strata_all <- sw %>%
      group_by(strata) %>%
      group_map(sample_strata) %>%
      ungroup()
    #
    # Left join sampled psu with data
    #
    x_sampled <- psu_strata_all %>%
      select(psu) %>%
      left_join(x, by = "psu")
    #
    # Select data for summary analysis. The summary() function should be tidyverse style if used
    # if no summary input, output raw bootstrapped df, otherwise output summarized results
    #
    if (is.null(summary)) {
      xBWS <- x_sampled
    }
    else{
      xBWS <- x_sampled %>%
        summary()
    }
    #
    # Get data for analysis
    # memory requirement can be an issue with very large group_by
    #
    xBWS$trial <- i
    result_df_list[[i]] <- xBWS
    psu_strata_all$trial <- i
    sampled_hh_df_list[[i]] <- psu_strata_all

    # xBWS
  }
  # TODO: create a class to contain these objects to enable foreach parallel loops: https://stackoverflow.com/questions/19791609/saving-multiple-outputs-of-foreach-dopar-loop

  # stopImplicitCluster()
  #
  # generate export, with options with output summary statistics
  #
  if (is.null(statistic)) {
    result_df <- bind_rows(result_df_list)
  }
  else{
    result_df <- bind_rows(result_df_list)
    result_df <- result_df %>%
      statistic()
  }
  sampled_hh_df <- bind_rows(sampled_hh_df_list)
  #
  # return a list of outputs
  # first one is full trip table of the replicates
  # second one is the households table of the replicates
  #
  output <- list('result_df' = result_df, 'sampled_hh_df' = sampled_hh_df)
  return(output)
}
