## ---- eval=FALSE---------------------------------------------------------
#  install.packages("mallet")

## ------------------------------------------------------------------------
library(mallet)

## ------------------------------------------------------------------------
library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578), readerControl = list(reader = readReut21578XMLasPlain))
reuters_text_vector <- unlist(lapply(reuters, as.character))

## ------------------------------------------------------------------------
stopwords_en <- system.file("stopwords/english.dat", package = "tm")

## ------------------------------------------------------------------------
mallet.instances <- mallet.import(id.array = as.character(1:length(reuters_text_vector)), 
                                  text.array = reuters_text_vector, 
                                  stoplist.file = stopwords_en,
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

## ------------------------------------------------------------------------
topic.model <- MalletLDA(num.topics=5, alpha.sum = 1, beta = 0.1)

## ------------------------------------------------------------------------
topic.model$loadDocuments(mallet.instances)

## ------------------------------------------------------------------------
vocabulary <- topic.model$getVocabulary()
head(vocabulary)

word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)

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
inspect(reuters[doc.topics[,1] > 0.05][1])

## ------------------------------------------------------------------------
usa_articles <- unlist(meta(reuters, "places")) == "usa"

usa.topic.words <- mallet.subset.topic.words(topic.model, 
                                              subset.docs = usa_articles,
                                              smoothed=TRUE, 
                                              normalized=TRUE)
other.topic.words <- mallet.subset.topic.words(topic.model, 
                                               subset.docs = !usa_articles,
                                               smoothed=TRUE, 
                                               normalized=TRUE)

## ------------------------------------------------------------------------
head(mallet.top.words(topic.model, usa.topic.words[1,]))
head(mallet.top.words(topic.model, other.topic.words[1,]))

