\name{mallet-package}
\alias{mallet-package}
\alias{mallet}
\docType{package}
\title{
An R wrapper for the Mallet topic modeling package
}
\description{
This package provides an interface to the Java implementation of latent Dirichlet allocation in the Mallet machine learning package. Mallet has many functions, this wrapper focuses on the topic modeling sub-package written by David Mimno. The package uses the \code{rJava} package to connect to a JVM.
}
\details{
\tabular{ll}{
Package: \tab mallet\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-08-08\cr
License: \tab MIT\cr
}

Create a topic model trainer: \code{\link{MalletLDA}}

Load documents from disk and import them:
\code{\link{mallet.read.dir}} 
\code{\link{mallet.import}}

Get info about word frequencies: \code{\link{mallet.word.freqs}}

Get trained model parameters:
\code{\link{mallet.doc.topics}}
\code{\link{mallet.topic.words}}
\code{\link{mallet.subset.topic.words}}

Reports on topic words:
\code{\link{mallet.top.words}}
\code{\link{mallet.topic.labels}}

Clustering of topics: \code{\link{mallet.topic.hclust}}

}
\author{
Maintainer: David Mimno
}
\references{
The model, Latent Dirichlet allocation (LDA):
\cite{David M Blei, Andrew Ng, Michael Jordan. Latent Dirichlet Allocation. J. of Machine Learning Research, 2003.}

The Java toolkit: 
\cite{Andrew Kachites McCallum. The Mallet Toolkit. 2002.}

Details of the fast sparse Gibbs sampling algorithm:
\cite{Limin Yao, David Mimno, Andrew McCallum. Streaming Inference for Latent Dirichlet Allocation. KDD, 2009.}

Hyperparameter optimization:
\cite{Hanna Wallach, David Mimno, Andrew McCallum. Rethinking LDA: Why Priors Matter. NIPS, 2010.}
}
