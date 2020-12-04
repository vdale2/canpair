

#' Cross Lagged Panel Model
#'
#'@Description A function to replace lengthy code for the cross lagged panel model analysis.
#'
#' @param df dataset
#' @param x2 current week variable 1
#' @param y2 current week variable 2
#' @param z2 current week variable 3
#' @param a2 current week variable 4
#' @param x1 last week variable 1
#' @param y1 last week variable 2
#' @param z1 last week variable 3
#' @param a1 last week variable 4
#'
#' @return
#' @export
#'
#' @examples cross_lagged_panel_model(dataset, attachment, desire, investment, mood, lastweekattachment, lastweekdesire, lastweekinvestment, lastweekmood)
cross_lagged_panel_model <- function(df, x2, y2, z2, a2, x1, y1, z1, a1) {
  null_mod_syntax <-
  x2 ~ 0*x1 + 0*y1 + 0*z1 + 0*a1
  y2 ~ 0*x1 + 0*y1 + 0*z1 + 0*a1
  z2 ~ 0*x1 + 0*y1 + 0*z1 + 0*a1
  a2 ~ 0*x1 + 0*y1 + 0*z1 + 0*a1

  full_mod_syntax <-
    x2 ~ x1 + y1 + z1 + a1
  y2 ~ x1 + y1 + z1 + a1
  z2 ~ x1 + y1 + z1 + a1
  a2 ~ x1 + y1 + z1 + a1

  m0 <- lavaan::sem(null_mod_syntax, data = df)
  m1 <- lavaan::sem(full_mod_syntax, data = df)

  lavaan::anova(m0, m1)

  lavaan::summary(m1)

  with(mydata, cor(data.frame(x2, y2, z2, a2)))

  subset(
    lavaan::parameterEstimates(m1),
    op == "~~" & lhs %in% c("x2", "y2", "z2", "a2")
  )

}
