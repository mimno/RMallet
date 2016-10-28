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

