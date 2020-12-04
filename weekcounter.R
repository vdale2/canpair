library(readr)
library(tidyverse)
library(psych)
library(dplyr)
library(data.table)


#' Week Counter
#'
#' @description The purpose of this function is to create a col in your dataset that will
#' infrom the researcher what week each row of a participants data was collected.
#'
#' @param df the name of your dataset
#' @param colnamer the name of the col that stores your participant IDs
#'
#' @return a "Weeknum" col that informs the researcher what week (1-25) each row of a participant's data was completed
#' @export
#'
#' @examples week_counter(dataset, dataset$participant_id_col)
week_counter <- function(df, colnamer) {
  df$Weeknum <- with(df, paste0('Week', ave(colnamer, colnamer, FUN = seq_along)))
  setDT(df)[, Weeknum := paste0('Week', seq_len(.N)), colnamer]
  view(df)
}





