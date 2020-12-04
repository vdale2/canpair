##function 4 - perform cross-lagged panel models, in which variables from
#last week are used to predict current variables
library(dplyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(psych)
library(DataCombine)
library(lavaan)

##create lag variables
#' Create Lag Variables for Cross Lag Panel Model
#' Description: Helper function to create variables for the cross lag panel model
#'
#' @param df dataframe
#' @param x first variable of interest
#' @param y second variable of interest
#' @param z third variable of interest
#' @param a fourth variable of interest
#'
#' @return four variables in which the data is extracted from the week before the current date
#' @export
#'
#' @examples last_week_col(mydata, attachment, attraction, mood, investment)


last_week_col <- function(df, x, y, z, a) {
  x1 <- lag(df$x)
  y1 <- lag(df$y)
  z1 <- lag(df$z)
  a1 <- lag(df$a)
}


