#creates a dataset including each participant only once, the row chosen for each
#participant is from their first weekly survey
library(dplyr)

#' Weekset Function
#' @description Creates a dataset that includes each participant only once, the row chosen for
#' each participant is from their first weekly survey
#' @param data your dataframe
#' @param id participant id col
#' @param date date col
#'
#' @return
#' @export
#'
#' @examples weekset(mydata, mydata$partID, mydata$dates)
weekset <- function(data, id, date) {
  data %>%
    group_by({{id}}) %>%
    slice(which.min(date)) -> new_dat
  view(new_dat)
}



