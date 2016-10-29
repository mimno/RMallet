context("mallet-train-model")

data(sotu)
stopwords_en <- system.file("stoplists/en.txt", package = "mallet")

test_that(desc="mallet.import",{
  expect_silent({
    sotu.instances <- 
      mallet.import(id.array = row.names(sotu), 
                    text.array = sotu[["text"]], 
                    stoplist = stopwords_en,
                    token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  })
  expect_is(sotu.instances, "jobjRef")
  
  expect_silent({
    sotu.instances <- 
      mallet.import(text.array = sotu[["text"]])
  })
  
})


test_that(desc="Train model",{
  
  expect_silent(
  sotu.instances <- 
    mallet.import(id.array = row.names(sotu), 
                  text.array = sotu[["text"]], 
                  stoplist = stopwords_en,
                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  )
  
  expect_silent(
    topic.model <- MalletLDA(num.topics=5, alpha.sum = 1, beta = 0.1)
  )
  
  expect_is(topic.model, "jobjRef")

  expect_silent(
    topic.model$loadDocuments(sotu.instances)
  )
  
  expect_silent(
    topic.model$train(20)
  )
})


