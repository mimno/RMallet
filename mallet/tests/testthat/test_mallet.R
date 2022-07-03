context("mallet-train-model")

data(sotu)

test_that(desc="mallet.import",{
  skip_on_cran()

  expect_silent({
    sotu.instances <-
      mallet.import(id.array = row.names(sotu),
                    text.array = sotu[["text"]],
                    stoplist = mallet_stoplist_file_path("en"),
                    token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  })
  expect_is(sotu.instances, "jobjRef")

  expect_silent({
    sotu.instances <-
      mallet.import(text.array = sotu[["text"]])
  })

})


test_that(desc="Train model",{
  skip_on_cran()

  expect_silent(
    sotu.instances <-
      mallet.import(id.array = row.names(sotu),
                    text.array = sotu[["text"]],
                    stoplist = mallet_stoplist_file_path("en"),
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


test_that(desc="Train model 2 (with stopwords)",{
  skip_on_cran()

  expect_silent(
    sotu.instances <-
      mallet.import(text.array = sotu[["text"]])
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

