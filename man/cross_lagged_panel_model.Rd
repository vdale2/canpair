% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_lagged_panel_model.R
\name{cross_lagged_panel_model}
\alias{cross_lagged_panel_model}
\title{Cross Lagged Panel Model}
\usage{
cross_lagged_panel_model(df, x2, y2, z2, a2, x1, y1, z1, a1)
}
\arguments{
\item{df}{dataset}

\item{x2}{current week variable 1}

\item{y2}{current week variable 2}

\item{z2}{current week variable 3}

\item{a2}{current week variable 4}

\item{x1}{last week variable 1}

\item{y1}{last week variable 2}

\item{z1}{last week variable 3}

\item{a1}{last week variable 4}
}
\value{

}
\description{
Cross Lagged Panel Model
}
\examples{
cross_lagged_panel_model(dataset, attachment, desire, investment, mood, lastweekattachment, lastweekdesire, lastweekinvestment, lastweekmood)
}
