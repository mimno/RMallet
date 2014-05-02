\name{mallet.topic.labels}
\alias{mallet.topic.labels}
\title{Get strings containing the most probable words for each topic}
\description{
This function returns a vector of strings, one for each topic, with the most probable words in that topic separated by spaces.
}
\usage{
mallet.topic.labels(topic.model, topic.words, num.top.words)
}
\arguments{
\item{topic.model}{The model returned by \code{MalletLDA}}
\item{topic.words}{The matrix of topic-word weights returned by \code{\link{mallet.topic.words}}}
\item{num.top.words}{The number of words to include for each topic}
}
\seealso{\code{\link{mallet.topic.words}} produces topic-word weights. \code{\link{mallet.top.words}} produces a data frame for a single topic.}
