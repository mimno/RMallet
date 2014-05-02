\name{mallet.topic.words}
\alias{mallet.topic.words}
\title{Retrieve a matrix of words weights for topics}
\description{
  This function returns a matrix with one row for every topic and one column for every word in the vocabulary.
}
\usage{
mallet.topic.words(topic.model, normalized, smoothed)
}
\arguments{
\item{topic.model}{The model returned by \code{MalletLDA}}
\item{normalized}{If true, normalize the rows so that each topic sums to one. If false, values will be integers (possibly plus the smoothing constant) representing the actual number of words of each type in the topics.}
\item{smoothed}{If true, add the smoothing parameter for the model (initial value specified as \code{beta} in \code{MalletLDA}). If false, many values will be zero.}
}
