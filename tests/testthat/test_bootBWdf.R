library(bbw)
context("Blocking weighted bootstrap")

# stratify by region
villageData$strata <- villageData$region

# define summarise function for each bootstraped sample
my_summarise <- function(df) {
  df %>%
    filter(!is.na(wash4)) %>%
    group_by(mMUAC) %>%
    summarise(mean_wash4 = round(mean(wash4), 0))
}

boot <- bootBW_df(
  x = indicatorsHH,
  sw = villageData,
  statistic = my_summarise,
  replicates = 9
)

test_that("boot is a data frame", {
  expect_is(boot, "data.frame")
})
