context("mallet-functionality")

data(sotu)
stopwords_en <- system.file("stoplists/en.txt", package = "mallet")

sotu.instances <- 
  mallet.import(id.array = row.names(sotu), 
                text.array = sotu[["text"]], 
                stoplist = stopwords_en,
                token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
topic.model$loadDocuments(sotu.instances)
topic.model$train(20)


test_that(desc="getVocabulary",{
  expect_silent(
    vocabulary <- topic.model$getVocabulary()
  )
  expect_equal(vocabulary[1:3], c("fellow-citizens", "senate", "house"))
})


test_that(desc="mallet.word.freqs",{
  expect_silent(
    word.freqs <- mallet.word.freqs(topic.model)
  )
  expect_equal(as.character(word.freqs[1:3,1]), c("fellow-citizens", "senate", "house"))
  expect_equal(word.freqs[1:3,2], c(16, 572, 485))
  expect_equal(word.freqs[1:3,3], c(16, 514, 435))
})


test_that(desc="setAlphaOptimization",{
  expect_silent(
    topic.model$setAlphaOptimization(20, 50)
  )
})

test_that(desc="train optimized",{
  expect_silent(
    topic.model$train(100)
  )
})


test_that(desc="maximize",{
  expect_silent(
    topic.model$maximize(10)
  )
})


test_that(desc="Get parameter matrices",{
  expect_silent(
    doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
  )
  
  expect_equal(dim(doc.topics), c(19254,10))
  expect_equal(object = rowSums(doc.topics), expected = rep(1,19254), tolerance = .00000000001, scale = 1)
  
  expect_silent(
    topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
  )
  
  expect_equal(dim(topic.words), c(10, 26311))
  expect_equal(object = rowSums(topic.words), expected = rep(1,10), tolerance = .00000000001, scale = 1)
  
})

test_that(desc="mallet.top.words",{
  expect_silent(
    top.words <- mallet.top.words(topic.model, word.weights = mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)[2,], num.top.words = 5)
  )
  expect_equal(dim(top.words), c(5,2))
  expect_lt(object = sum(top.words$weights), expected = 1)
})


test_that(desc="mallet.subset",{
  modern_times <- sotu$year > 1899
  expect_silent({
    modern.topic.words <- mallet.subset.topic.words(topic.model, 
                                                 subset.docs = modern_times,
                                                 smoothed=TRUE, 
                                                 normalized=TRUE)
    not.modern.topic.words <- mallet.subset.topic.words(topic.model, 
                                                   subset.docs = !modern_times,
                                                   smoothed=TRUE, 
                                                   normalized=TRUE)
  })
  
  expect_equal(dim(modern.topic.words), c(10, 26311))
  expect_equal(dim(not.modern.topic.words), c(10, 26311))
  expect_true(any(modern.topic.words != not.modern.topic.words))
})

