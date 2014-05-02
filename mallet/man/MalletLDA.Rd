\name{MalletLDA}
\alias{MalletLDA}
\title{Create a Mallet topic model trainer}
\description{
This function creates a java cc.mallet.topics.RTopicModel object that wraps a Mallet topic model trainer java object,
cc.mallet.topics.ParallelTopicModel.
Note that you can call any of the methods of this java object as properties. In the example below, I make a call directly to the \code{topic.model$setAlphaOptimization(20, 50)} java method, which passes this update to the model itself.
}
\usage{MalletLDA(num.topics, alpha.sum, beta)}
\arguments{
\item{num.topics}{The number of topics to use. If not specified, this defaults to 10.}
\item{alpha.sum}{This is the magnitude of the Dirichlet prior over the topic distribution of a document. The default value is 5.0. With 10 topics, this setting leads to a Dirichlet with parameter \eqn{\alpha_k = 0.5}. You can intuitively think of this parameter as a number of "pseudo-words", divided evenly between all topics, that are present in every document no matter how the other words are allocated to topics. This is an initial value, which may be changed during training if hyperparameter optimization is active.}
\item{beta}{This is the per-word weight of the Dirichlet prior over topic-word distributions. The magnitude of the distribution (the sum over all words of this parameter) is determined by the number of words in the vocabulary. Again, this value may change due to hyperparameter optimization.}
}
\examples{
\dontrun{
library(mallet)

## Create a wrapper for the data with three elements, one for each column.
##  R does some type inference, and will guess wrong, so give it hints with "colClasses".
##  Note that "id" and "text" are special fields -- mallet will look there for input.
##  "class" is arbitrary. We will only use that field on the R side.
documents <- read.table("nips_cvpr.txt", col.names=c("id", "class", "text"),
	     		colClasses=rep("character", 3), sep="\t", quote="")

## Create a mallet instance list object. Right now I have to specify the stoplist
##  as a file, I can't pass in a list from R.
## This function has a few hidden options (whether to lowercase, how we 
##   define a token). See ?mallet.import for details.
mallet.instances <- mallet.import(documents$id, documents$text, "en.txt",
		    		token.regexp = "\\\\p{L}[\\\\p{L}\\\\p{P}]+\\\\p{L}")

## Create a topic trainer object.
topic.model <- MalletLDA(num.topics=20)

## Load our documents. We could also pass in the filename of a 
##  saved instance list file that we build from the command-line tools.
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
##  These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations, 
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
##  We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(200)

## NEW: run through a few iterations where we pick the best topic for each token, 
##  rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities, 
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

## What are the top words in topic 7?
##  Notice that R indexes from 1, so this will be the topic that mallet called topic 6.
mallet.top.words(topic.model, topic.words[7,])

## Show the first few documents with at least 5% topic 7 and 5% topic 10
head(documents[ doc.topics[7,] > 0.05 & doc.topics[10,] > 0.05, ])

## How do topics differ across different sub-corpora?
nips.topic.words <- mallet.subset.topic.words(topic.model, documents$class == "NIPS",
		    					   smoothed=T, normalized=T)
cvpr.topic.words <- mallet.subset.topic.words(topic.model, documents$class == "CVPR",
		    					   smoothed=T, normalized=T)

## How do they compare?
mallet.top.words(topic.model, nips.topic.words[10,])
mallet.top.words(topic.model, cvpr.topic.words[10,])
}
}