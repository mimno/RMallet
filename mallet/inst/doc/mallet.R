## ---- eval=FALSE---------------------------------------------------------
#  install.packages("mallet")

## ------------------------------------------------------------------------
library(mallet)

## ------------------------------------------------------------------------
library(dplyr)
data(sotu)
sotu[["text"]][1:2]

## ------------------------------------------------------------------------
dir(system.file("stoplists/", package = "mallet"))
stopwords_en <- system.file("stoplists/en.txt", package = "mallet")

## ------------------------------------------------------------------------
sotu.instances <- 
  mallet.import(id.array = row.names(sotu), 
                text.array = sotu[["text"]], 
                stoplist = stopwords_en,
                token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

## ------------------------------------------------------------------------
sotu.instances.short <- 
  mallet.import(text.array = sotu[["text"]])

## ------------------------------------------------------------------------
stop_vector <- readLines(stopwords_en)
sotu.instances.short <- 
  mallet.import(text.array = sotu[["text"]], 
                stoplist = stop_vector)

## ------------------------------------------------------------------------
topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)

## ------------------------------------------------------------------------
topic.model$loadDocuments(sotu.instances)

## ------------------------------------------------------------------------
vocabulary <- topic.model$getVocabulary()
head(vocabulary)

word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)

## ------------------------------------------------------------------------
topic.model$setAlphaOptimization(20, 50)

## ------------------------------------------------------------------------
topic.model$train(200)

## ------------------------------------------------------------------------
topic.model$maximize(10)

## ------------------------------------------------------------------------
doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

## ------------------------------------------------------------------------
mallet.top.words(topic.model, word.weights = topic.words[2,], num.top.words = 5)

## ------------------------------------------------------------------------
sotu[["text"]][doc.topics[,2] > 0.05][1]

## ------------------------------------------------------------------------
state_file <- file.path(tempdir(), "temp_mallet_state.gz")
save.mallet.state(topic.model = topic.model, state.file = state_file)

## ------------------------------------------------------------------------
doc.topics.counts <- mallet.doc.topics(topic.model, smoothed=FALSE, normalized=FALSE)

rm(topic.model)

## ------------------------------------------------------------------------
new.topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
new.topic.model$loadDocuments(sotu.instances)
load.mallet.state(topic.model = new.topic.model, state.file = state_file)

doc.topics.counts[1:3, 1:10]
mallet.doc.topics(new.topic.model, smoothed=FALSE, normalized=FALSE)[1:3, 1:10]

