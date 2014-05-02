\name{mallet.word.freqs}
\alias{mallet.word.freqs}
\title{Descriptive statistics of word frequencies}
\description{
This method returns a data frame with one row for each unique vocabulary word, and three columns: the word as a \code{character} value, the total number of tokens of that word type, and the total number of documents that contain that word at least once. This information can be useful in identifying candidate stopwords.
}
\usage{
mallet.word.freqs(topic.model)
}
\arguments{
\item{topic.model}{A Mallet topic trainer returned by \code{MalletLDA}}
}
\seealso{\code{\link{MalletLDA}}}
