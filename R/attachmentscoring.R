#' Attahchment Anxiety and Avoidance Measurement Tool
#'
#'
#' @description The purpose of this function is to score participants responses to the 12-item
#' Wei et. al Attachment Measure and provide their attachment anxiety and attachment avoidance scores.
#'
#' @param df dataframe
#' @param firstq first col in the series of attachment style cols
#'
#' @return df with new cols for attachment anxiety and attachment avoidance scores
#' @export
#'
#' @examples
#' attachment_tool(mydata, "attachmentQ1")
  attachment_tool <- function(df, firstq) {
  itemnum <- 12

  fistcol <- match(firstq, names(df))
  lastcol <- firstcol+itemnum - 1
  varnames <- names(df[firstcol:lastcol])

  varnames_scoring<-varnames
  varnames_scoring[[1]]<-paste("-", varnames_scoring[[1]], sep = "")
  varnames_scoring[[5]]<-paste("-", varnames_scoring[[5]], sep = "")
  varnames_scoring[[8]]<-paste("-", varnames_scoring[[8]], sep = "")
  varnames_scoring[[9]]<-paste("-", varnames_scoring[[9]], sep = "")

  attachment.anxiety <- varnames_scoring[2, 4, 6, 8, 10, 12 ]
  attachment.avoidance <- varnames_scoring[1, 3, 5, 7, 9, 11]

  attachment.anxiety.scores <- psych::scoreItems(attachment.anxiety, dyplr::select(df, !!atacchment.anxiety), totals = FALSE, min = 1, max = 8)
  df <- cbind(df, attachment.anxiety.scores$scores)
  names(df)[names(df)==names(df[ncol(df)])] <- "Attachment Anxiety"

  attachment.avoidance.scores <- psych::scoreItems(attachment.avoidance, dyplr::select(df, !!attachment.avoidance), totals = FALSE, min = 1, max = 8)
  df <- cbind(df, attachment.avoidance.scores$scores)
  names(df)[names(df)==names(df[ncol(df)])] <- "Attachment Avoidance"

  return(df)
  itemcorr <- varnames %>%
    colMeans(na.rm = TRUE)
  print(itemcorr)
  cronbachs <- psych::alpha(varnames)
  print(cronbachs)


}
