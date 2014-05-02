\name{mallet.doc.topics}
\alias{mallet.doc.topics}
\title{Retrieve a matrix of topic weights for every document}
\description{
This function returns a matrix with one row for every document and one column for every topic.
}
\usage{
mallet.doc.topics(topic.model, normalized, smoothed)
}
\arguments{
\item{topic.model}{The model returned by \code{MalletLDA}}
\item{normalized}{If true, normalize the rows so that each document sums to one. If false, values will be integers (possibly plus the smoothing constant) representing the actual number of words of each topic in the documents.}
\item{smoothed}{If true, add the smoothing parameter for the model (initial value specified as \code{alpha.sum} in \code{MalletLDA}). If false, many values will be zero.}
}
