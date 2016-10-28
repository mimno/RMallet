context("mallet")
library(tm)
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578), readerControl = list(reader = readReut21578XMLasPlain))
reuters_text_vector <- unlist(lapply(reuters, as.character))
stopwords_en <- system.file("stopwords/english.dat", package = "tm")

test_that(desc="mallet.import",{
  expect_silent({
    mallet.instances <- mallet.import(id.array = as.character(1:length(reuters_text_vector)), 
                                      text.array = reuters_text_vector, 
                                      stoplist.file = stopwords_en,
                                      token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  })
  expect_is(mallet.instances, "jobjRef")
})

mallet.instances <- mallet.import(id.array = as.character(1:length(reuters_text_vector)), 
                                  text.array = reuters_text_vector, 
                                  stoplist.file = stopwords_en,
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

test_that(desc="MalletLDA",{
  expect_that({
    topic.model <- MalletLDA(num.topics=5, alpha.sum = 1, beta = 0.1)
    }, condition = not(throws_error())
  )
  expect_is(topic.model, "jobjRef")
})

topic.model <- MalletLDA(num.topics=5, alpha.sum = 1, beta = 0.1)

test_that(desc="loadDocuments",{
  expect_that({
    topic.model$loadDocuments(mallet.instances)
  }, condition = not(throws_error())
  )
})


test_that(desc="getVocabulary",{
  expect_that({
    vocabulary <- topic.model$getVocabulary()
  }, condition = not(throws_error())
  )
  expect_equal(vocabulary[1:3], c("diamond", "shamrock", "corp"))
})

test_that(desc="mallet.word.freqs",{
  expect_that({
    word.freqs <- mallet.word.freqs(topic.model)
  }, condition = not(throws_error())
  )
  expect_equal(as.character(word.freqs[1:3,1]), c("diamond", "shamrock", "corp"))
  expect_equal(word.freqs[1:3,2], c(0,1,4))
  expect_equal(word.freqs[1:3,3], c(0,1,2))
})


test_that(desc="setAlphaOptimization",{
  expect_that({
    topic.model$setAlphaOptimization(20, 50)
  }, condition = not(throws_error())
  )
})


test_that(desc="train",{
  expect_that({
    topic.model$train(200)
  }, condition = not(throws_error())
  )
})


test_that(desc="train",{
  expect_that({
    topic.model$maximize(10)
  }, condition = not(throws_error())
  )
})


test_that(desc="get parameter matrices",{
  expect_that({
    doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
  }, condition = not(throws_error()))
  
  expect_equal(dim(doc.topics), c(20,5))
  expect_equal(object = rowSums(doc.topics), expected = rep(1,20), tolerance = .00000000001, scale = 1)

  expect_that({
    topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
  }, condition = not(throws_error()))

  expect_equal(dim(topic.words), c(5,959))
  expect_equal(object = rowSums(topic.words), expected = rep(1,5), tolerance = .00000000001, scale = 1)
  
})

test_that(desc="mallet.top.words",{
  expect_that({
    top.words <- mallet.top.words(topic.model, word.weights = mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)[2,], num.top.words = 5)
  }, condition = not(throws_error()))
  
  expect_equal(dim(top.words), c(5,2))
  expect_less_than(object = sum(top.words$weights), expected = 1)
})


test_that(desc="mallet.subset",{
  usa_articles <- unlist(meta(reuters, "places")) == "usa"
  expect_that({
    usa.topic.words <- mallet.subset.topic.words(topic.model, 
                                                 subset.docs = usa_articles,
                                                 smoothed=TRUE, 
                                                 normalized=TRUE)
    other.topic.words <- mallet.subset.topic.words(topic.model, 
                                                   subset.docs = !usa_articles,
                                                   smoothed=TRUE, 
                                                   normalized=TRUE)
  }, condition = not(throws_error()))
  
  expect_equal(dim(usa.topic.words), c(5,959))
  expect_equal(dim(other.topic.words), c(5,959))
  expect_that(other.topic.words, not(is_equivalent_to(usa.topic.words)))
})


