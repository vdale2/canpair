
#' Investments So Far
#'
#' @description The purpose of this function is to determine how many investments participants have made
#' so far at any given week in the Canada Pair Project
#'
#' @param df the name of your dataframe
#' @param firstq the name of your first weekly investment col (going in chronoloigical order from left to right)
#' @param numinvest the number of weekly investment cols you have in your dataframe
#'
#' @return a invests.so.far col that will tell the researcher how many investments each participant has
#' made into their relationship at any given week during the study.
#' @export
#'
#' @examples investments.so.far(mydata, met_friends, 5)
investments.so.far <- function(df, firstq, numinvest) {
 itemnum = numinvest
 firstcol <- match(firstq, names(df))
 firstcol[is.na(firstcol)] <- 0
 lastcol <- firstcol + itemnum  - 1
 newframe <- select(df, firstcol:lastcol)
 invests.so.far <- cbind(total = rowSums(newframe))
 df$invests.so.far <- invests.so.far
 view(df)
}

