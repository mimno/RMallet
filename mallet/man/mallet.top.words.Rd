\name{mallet.top.words}
\alias{mallet.top.words}
\title{Get the most probable words and their probabilities for one topic.}
\description{
This function returns a data frame with two columns, one containing the most probable words as character values, the second containing the weight assigned to that word in the word weights vector you supplied.
}
\usage{
mallet.top.words(topic.model, word.weights, num.top.words)
}
\arguments{
\item{topic.model}{The model returned by \code{MalletLDA}}
\item{word.weights}{A vector of word weights for one topic, usually a row from the \code{topic.words} matrix from \code{mallet.topic.words}.}
\item{num.top.words}{The number of most probable words to return. If not specified, defaults to 10.}
}
