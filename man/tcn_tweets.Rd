% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcn_tweets.R
\name{tcn_tweets}
\alias{tcn_tweets}
\title{Get tweets}
\usage{
tcn_tweets(tweet_ids = NULL, token = NULL)
}
\arguments{
\item{tweet_ids}{List. Tweet ids or tweet URLs.}

\item{token}{List. Twitter API tokens.}
}
\value{
A named list. Dataframes of tweets and users.
}
\description{
Collects tweets for a list of tweet ids.
}
\examples{
\dontrun{
# get twitter conversation threads by tweet ids or urls
tweet_ids <- c("xxxxxxxx",
               "https://twitter.com/xxxxxxxx/status/xxxxxxxx")
tweets <- tcn_tweets(tweet_ids, token)
}

}
