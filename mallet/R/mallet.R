#' @title 
#' An R Wrapper for the Mallet Topic Modeling Package
#' 
#' @description
#' This package provides an interface to the Java implementation of latent 
#' Dirichlet allocation in the Mallet machine learning package. Mallet has many 
#' functions, this wrapper focuses on the topic modeling sub-package written by 
#' David Mimno. The package uses the \code{rJava} package to connect to a JVM.
#' 
#' @details 
#' \tabular{ll}{
#' Package: \tab mallet\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2013-08-08\cr
#' License: \tab MIT\cr
#' }
#' 
#' Create a topic model trainer: 
#' \code{\link{MalletLDA}}
#' 
#' Load documents from disk and import them:
#' \code{\link{mallet.read.dir}} 
#' \code{\link{mallet.import}}
#' 
#' Get info about word frequencies: 
#' \code{\link{mallet.word.freqs}}
#' 
#' Get trained model parameters:
#' \code{\link{mallet.doc.topics}}
#' \code{\link{mallet.topic.words}}
#' \code{\link{mallet.subset.topic.words}}
#' 
#' Reports on topic words:
#' \code{\link{mallet.top.words}}
#' \code{\link{mallet.topic.labels}}
#' 
#' Clustering of topics: 
#' \code{\link{mallet.topic.hclust}}
#' 
#' @author 
#' Maintainer: David Mimno
#' 
#' @references 
#' The model, Latent Dirichlet allocation (LDA):
#' \cite{David M Blei, Andrew Ng, Michael Jordan. Latent Dirichlet Allocation. J. of Machine Learning Research, 2003.}
#' 
#' The Java toolkit: 
#' \cite{Andrew Kachites McCallum. The Mallet Toolkit. 2002.}
#' 
#' Details of the fast sparse Gibbs sampling algorithm:
#' \cite{Limin Yao, David Mimno, Andrew McCallum. Streaming Inference for Latent Dirichlet Allocation. KDD, 2009.}
#' 
#' Hyperparameter optimization:
#' \cite{Hanna Wallach, David Mimno, Andrew McCallum. Rethinking LDA: Why Priors Matter. NIPS, 2010.}
#' 
#' @name mallet
#' @docType package
#' @import rJava
NULL

#' @title 
#' Create a Mallet topic model trainer
#' 
#' @description 
#' This function creates a java cc.mallet.topics.RTopicModel object that wraps a 
#' Mallet topic model trainer java object, cc.mallet.topics.ParallelTopicModel. 
#' Note that you can call any of the methods of this java object as properties. 
#' In the example below, I make a call directly to the 
#' \code{topic.model$setAlphaOptimization(20, 50)} java method, 
#' which passes this update to the model itself.
#'
#' @param num.topics
#' The number of topics to use. If not specified, this defaults to 10.
#' @param alpha.sum
#' This is the magnitude of the Dirichlet prior over the topic distribution of a document. 
#' The default value is 5.0. With 10 topics, this setting leads to a Dirichlet with 
#' parameter \eqn{\alpha_k = 0.5}. You can intuitively think of this parameter as a 
#' number of "pseudo-words", divided evenly between all topics, that are present in 
#' every document no matter how the other words are allocated to topics. This is an 
#' initial value, which may be changed during training if hyperparameter 
#' optimization is active.
#' @param beta
#' This is the per-word weight of the Dirichlet prior over topic-word distributions. 
#' The magnitude of the distribution (the sum over all words of this parameter) is 
#' determined by the number of words in the vocabulary. Again, this value may change
#' due to hyperparameter optimization.
#'
#' @examples 
#' \dontrun{
#' library(mallet)
#' 
#' ## Create a wrapper for the data with three elements, one for each column.
#' ##  R does some type inference, and will guess wrong, so give it hints with "colClasses".
#' ##  Note that "id" and "text" are special fields -- mallet will look there for input.
#' ##  "class" is arbitrary. We will only use that field on the R side.
#' documents <- read.table("nips_cvpr.txt", col.names=c("id", "class", "text"),
#'                         colClasses=rep("character", 3), sep="\t", quote="")
#'
#' ## Create a mallet instance list object. Right now I have to specify the stoplist
#' ##  as a file, I can't pass in a list from R.
#' ## This function has a few hidden options (whether to lowercase, how we 
#' ##   define a token). See ?mallet.import for details.
#' mallet.instances <- mallet.import(documents$id, documents$text, "en.txt",
#'                                   token.regexp = "\\\\p{L}[\\\\p{L}\\\\p{P}]+\\\\p{L}")
#'
#' ## Create a topic trainer object.
#' topic.model <- MalletLDA(num.topics=20)
#' 
#' ## Load our documents. We could also pass in the filename of a 
#' ##  saved instance list file that we build from the command-line tools.
#' topic.model$loadDocuments(mallet.instances)
#' 
#' ## Get the vocabulary, and some statistics about word frequencies.
#' ##  These may be useful in further curating the stopword list.
#' vocabulary <- topic.model$getVocabulary()
#' word.freqs <- mallet.word.freqs(topic.model)
#' 
#' ## Optimize hyperparameters every 20 iterations, 
#' ##  after 50 burn-in iterations.
#' topic.model$setAlphaOptimization(20, 50)
#' 
#' ## Now train a model. Note that hyperparameter optimization is on, by default.
#' ##  We can specify the number of iterations. Here we'll use a large-ish round number.
#' topic.model$train(200)
#' 
#' ## NEW: run through a few iterations where we pick the best topic for each token, 
#' ##  rather than sampling from the posterior distribution.
#' topic.model$maximize(10)
#' 
#' ## Get the probability of topics in documents and the probability of words in topics.
#' ## By default, these functions return raw word counts. Here we want probabilities, 
#' ##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
#' doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
#' topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
#' 
#' ## What are the top words in topic 7?
#' ##  Notice that R indexes from 1, so this will be the topic that mallet called topic 6.
#' mallet.top.words(topic.model, topic.words[7,])
#' 
#' ## Show the first few documents with at least 5% topic 7 and 5% topic 10
#' head(documents[ doc.topics[7,] > 0.05 & doc.topics[10,] > 0.05, ])
#' 
#' ## How do topics differ across different sub-corpora?
#' nips.topic.words <- mallet.subset.topic.words(topic.model, documents$class == "NIPS",
#'                                               smoothed=T, normalized=T)
#' cvpr.topic.words <- mallet.subset.topic.words(topic.model, documents$class == "CVPR",
#'                                               smoothed=T, normalized=T)
#' 
#' ## How do they compare?
#' mallet.top.words(topic.model, nips.topic.words[10,])
#' mallet.top.words(topic.model, cvpr.topic.words[10,])
#' }
#' 
#' @export
MalletLDA <- function(num.topics = 10, alpha.sum = 5.0, beta = 0.01) { rJava::.jnew("cc/mallet/topics/RTopicModel", num.topics, alpha.sum, beta) }




#' @title 
#' Retrieve a matrix of words weights for topics
#' 
#' @description 
#' This function returns a matrix with one row for every topic 
#' and one column for every word in the vocabulary.
#' 
#' @param topic.model
#' The model returned by \code{MalletLDA}
#' @param normalized
#' If \code{TRUE}, normalize the rows so that each topic sums to one. If \code{FALSE}, 
#' values will be integers (possibly plus the smoothing constant) representing the 
#' actual number of words of each type in the topics.
#' @param smoothed
#' If \code{TRUE}, add the smoothing parameter for the model (initial value specified as 
#' \code{beta} in \code{MalletLDA}). If \code{FALSE}, many values will be zero.
#' 
#' @export
mallet.topic.words <- function(topic.model, normalized=FALSE, smoothed=FALSE) {
  rJava::.jevalArray(topic.model$getTopicWords(normalized, smoothed), simplify=T) 
}


#' @title 
#' Retrieve a matrix of topic weights for every document
#' 
#' @description 
#' This function returns a matrix with one row for every document and one 
#' column for every topic.
#' 
#' @param topic.model
#' The model returned by \code{MalletLDA}
#' @param normalized
#' If \code{TRUE}, normalize the rows so that each document sums to one. If \code{FALSE}, 
#' values will be integers (possibly plus the smoothing constant) representing the 
#' actual number of words of each topic in the documents.
#' @param smoothed
#' If \code{TRUE}, add the smoothing parameter for the model (initial value specified as 
#' \code{alpha.sum} in \code{MalletLDA}). If \code{FALSE}, many values will be zero.
#' 
#' @export
mallet.doc.topics <- function(topic.model, normalized=FALSE, smoothed=FALSE) { 
  rJava::.jevalArray(topic.model$getDocumentTopics(normalized, smoothed), simplify=T)
}


#' @title 
#' Descriptive statistics of word frequencies
#' 
#' @description 
#' This method returns a data frame with one row for each unique vocabulary word, 
#' and three columns: the word as a \code{character} value, the total number of 
#' tokens of that word type, and the total number of documents that contain that 
#' word at least once. This information can be useful in identifying candidate 
#' stopwords.
#' 
#' @param topic.model
#' A Mallet topic trainer returned by \code{MalletLDA}
#' 
#' @seealso 
#' \code{\link{MalletLDA}}
#' 
#' @export
mallet.word.freqs <- function(topic.model) {
  word.freqs <- rJava::.jevalArray(topic.model$getWordFrequencies(), simplify=T)
  data.frame(words = topic.model$getVocabulary(), term.freq = word.freqs[,1], doc.freq = word.freqs[,2])
}


#' @title 
#' Estimate topic-word distributions from a sub-corpus
#' 
#' @description 
#' This function returns a matrix of word probabilities for each topic similar to 
#' \code{\link{mallet.topic.words}}, but estimated from a subset of the documents 
#' in the corpus. The model assumes that topics are the same no matter where they 
#' are used, but we know this is often not the case. This function lets us test 
#' whether some words are used more or less than we expect in a particular set 
#' of documents.
#' 
#' @param topic.model
#' The model returned by \code{MalletLDA}
#' @param subset.docs 
#' An array of \code{TRUE}/\code{FALSE} values specifying which documents should 
#' be used and which should be ignored.
#' @param normalized 
#' If \code{TRUE}, normalize the rows so that each topic sums to one. If \code{FALSE}, 
#' values will be integers (possibly plus the smoothing constant) representing 
#' the actual number of words of each type in the topics.
#' @param smoothed 
#' If \code{TRUE}, add the smoothing parameter for the model (initial value specified 
#' as \code{beta} in \code{MalletLDA}). If \code{FALSE}, many values will be zero.
#' 
#' @seealso 
#' \code{\link{mallet.topic.words}}
#' 
#' @examples 
#' \dontrun{
#' nips.topic.words <- 
#'   mallet.subset.topic.words(topic.model, documents$class == "NIPS", smoothed=T, normalized=T)
#' }
#' 
#' @export
mallet.subset.topic.words <- function(topic.model, subset.docs, normalized=FALSE, smoothed=FALSE) {
  rJava::.jevalArray(topic.model$getSubCorpusTopicWords(subset.docs, normalized, smoothed), simplify=T)
}


#' @title 
#' Get the most probable words and their probabilities for one topic
#' 
#' @description 
#' This function returns a data frame with two columns, one containing the most 
#' probable words as character values, the second containing the weight assigned 
#' to that word in the word weights vector you supplied.
#' 
#' @param topic.model
#' The model returned by \code{MalletLDA}
#' @param word.weights 
#' A vector of word weights for one topic, usually a row from the \code{topic.words} 
#' matrix from \code{mallet.topic.words}.
#' @param num.top.words
#' The number of most probable words to return. If not specified, defaults to 10.
#' 
#' @export
mallet.top.words <- function(topic.model, word.weights, num.top.words=10) {
  top.indices <- order(word.weights, decreasing=T)[1:num.top.words]
  data.frame(words = topic.model$getVocabulary()[top.indices], weights = word.weights[top.indices], stringsAsFactors=F)
}

#' @title 
#' Import text documents into Mallet format
#' 
#' @description 
#' This function takes an array of document IDs and text files (as character strings) 
#' and converts them into a Mallet instance list.
#' 
#' @param id.array
#' An array of document IDs.
#' @param text.array
#' An array of text strings to use as documents. The type of the array must be \code{character}.
#' @param stoplist.file
#' The name of a file containing stopwords (words to ignore), one per line. 
#' If the file is not in the current working directory, you may need to include a full path.
#' @param preserve.case
#' By default, the input text is converted to all lowercase
#' @param token.regexp
#' A quoted string representing a regular expression that defines a token. The default 
#' is one or more unicode letter: "[\\\\p\{L\}]+". Note that special characters must 
#' have double backslashes.
#' 
#' @seealso 
#' \code{\link{mallet.word.freqs}} returns term and document frequencies, which may be useful in selecting stopwords.
#' 
#' @examples 
#' \dontrun{
#' mallet.instances <- mallet.import(documents$id, documents$text, "en.txt",
#'                                   token.regexp = "\\\\p{L}[\\\\p{L}\\\\p{P}]+\\\\p{L}")
#' }
#' 
#' @export
mallet.import <- function(id.array, text.array, stoplist.file, preserve.case=FALSE, token.regexp="[\\p{L}]+") {
  stoplist.file <- normalizePath(stoplist.file)
  if (class(text.array[1]) != "character") stop("Text field is not a string. Remember to create data frames with stringsAsFactors=F.")
  token.pattern <- rJava::J("java/util/regex/Pattern")$compile(token.regexp)
  pipe.list <- rJava::.jnew("java/util/ArrayList")
  pipe.list$add(rJava::.jnew("cc/mallet/pipe/CharSequence2TokenSequence", token.pattern))
  if (! preserve.case) { pipe.list$add(rJava::.jnew("cc/mallet/pipe/TokenSequenceLowercase")) }
  pipe.list$add(rJava::.jnew("cc/mallet/pipe/TokenSequenceRemoveStopwords", rJava::.jnew("java/io/File", stoplist.file), "UTF-8", FALSE, FALSE, FALSE))
  pipe.list$add(rJava::.jnew("cc/mallet/pipe/TokenSequence2FeatureSequence"))
  #pipe.list$add(rJava::.jnew("cc/mallet/pipe/PrintInputAndTarget"))

  pipe <- rJava::.jnew("cc/mallet/pipe/SerialPipes", rJava::.jcast(pipe.list, "java/util/Collection"))

  instances <- rJava::.jnew("cc/mallet/types/InstanceList", rJava::.jcast(pipe, "cc/mallet/pipe/Pipe"))

  rJava::J("cc/mallet/topics/RTopicModel")$addInstances(instances, id.array, text.array)

  return(instances)
}

# mallet.read.dir() function, created by Dan Bowen
# This function takes a directory path as its only argument
# ... and returns a data.frame() with 2 columns: <id> & <text>.
# ... This data.frame() has as many rows as there are files in the Dir.
# The form of this functions return attempts to conform to that
# ... used by the mallet.import() function, available in the 'mallet' R package

#' @title 
#' Import documents from a directory into Mallet format
#' 
#' @author Dan Bowen
#' 
#' @description 
#'  This function takes a directory path as its only argument and returns a 
#'  \code{data.frame} with two columns: <id> & <text>,
#'  which can be passed to the \code{mallet.import} function.
#'  This \code{data.frame} has as many rows as there are files in the \code{Dir}.
#' 
#' @param Dir
#' The path to a directory containing one document per file.
#' 
#' @note 
#' This function was contributed to RMallet by Dan Bowen. 
#' 
#' @seealso 
#' \code{\link{mallet.import}}
#' 
#' @examples 
#' \dontrun{
#' documents <- mallet.read.dir(Dir)
#' mallet.instances <- 
#'   mallet.import(documents$id, documents$text, "en.txt", 
#'                 token.regexp = "\\\\p{L}[\\\\p{L}\\\\p{P}]+\\\\p{L}")
#' }
#' 
#' @export
mallet.read.dir <- function(Dir) {
  # get Dir Files (filepaths)
  Files <- file.path(Dir, list.files(Dir))
  # for each File:
  mallet.read.file <- function(File) {
    # read File, per line
    Lines <- scan(File, what='character', sep='\n', quote='')
    # paste Lines back together with '\n'
    string <- paste(Lines, collapse='\n')
    # return data.frame
    data.frame(id=File, text=string, stringsAsFactors=F)
  }
  # apply the above function to the Files in the dir
  # ... rbind the resulting list of data.frames together
  do.call(rbind, lapply(Files, mallet.read.file))
}

## Get a vector containing short names for all the topics

#' @title 
#' Get strings containing the most probable words for each topic
#' 
#' @description 
#' This function returns a vector of strings, one for each topic, with the 
#' most probable words in that topic separated by spaces.
#'
#' @param topic.model
#' The model returned by \code{MalletLDA}
#' @param topic.words
#' The matrix of topic-word weights returned by \code{\link{mallet.topic.words}}
#' @param num.top.words
#' The number of words to include for each topic
#' 
#' @seealso 
#' \code{\link{mallet.topic.words}} produces topic-word weights. 
#' \code{\link{mallet.top.words}} produces a data frame for a single topic.
#' 
#' @export
mallet.topic.labels <- function(topic.model, topic.words, num.top.words=3) {
  n.topics <- dim(topic.words)[1]
  topics.labels <- rep("", n.topics)
  for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words)$words, collapse=" ")
  topics.labels
}

## Return a hierarchical clustering of topics.

#' @title 
#' Return a hierarchical clustering of topics
#' 
#' @description 
#' Returns a hierarchical clustering of topics that can be plotted as a dendrogram. 
#' There are two ways of measuring topic similarity: topics may contain the some of 
#' the same words, or the may appear in some of the same documents. The \code{balance} parameter allows you to interpolate between the similarities determined by these two methods.
#' 
#' @param doc.topics
#' A documents by topics matrix of topic probabilities.
#' @param topic.words
#' A topics by words matrix of word probabilities.
#' @param balance
#' A value between 0.0 (use only document-level similarity) 
#' and 1.0 (use only word-level similarity).
#' @param ... 
#' Further arguments for \code{\link[stats]{hclust}}.
#' 
#' @seealso 
#' This function uses data matrices from \code{\link{mallet.doc.topics}} 
#' and \code{\link{mallet.topic.words}}
#' 
#' @examples 
#' \dontrun{
#' topic.labels <- mallet.topic.labels(topic.model, topic.words, 3)
#' plot(mallet.topic.hclust(doc.topics, topic.words, 0.3), labels=topic.labels)
#' }
#' 
#' @export
mallet.topic.hclust <- function(doc.topics, topic.words, balance = 0.3) {
  ## transpose and normalize the doc topics
  topic.docs <- t(doc.topics)
  topic.docs <- topic.docs / rowSums(topic.docs)

  hclust(balance * dist(topic.words) + (1.0 - balance) * dist(topic.docs))
}


mallet.save.instances <- function(instances, filename) {
  instances$save(rJava::.jnew("java/io/File", filename))
}

mallet.load <- function(filename) {
  rJava::J("cc.mallet.types.InstanceList")$load(rJava::.jnew("java/io/File", filename))
}
