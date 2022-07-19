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


test_that(desc="Issue #2 bug",{
  skip_on_cran()

  documents <- data.frame(id=as.character(1:5), text=c("this is an example document",
                                                       "here is another example document",
                                                       "another small example",
                                                       "this document is quite long compared to the other examples",
                                                       "finally here is the last example"),
                        stringsAsFactors=FALSE)


  mallet.instances <- mallet.import(documents$id, documents$text,
                                    stoplist = c("the", "of"),
                                    token.regexp = "[\\p{L}]+")
  topic.model <- MalletLDA(num.topics=2)
  topic.model$loadDocuments(mallet.instances)
  vocabulary <- topic.model$getVocabulary()

  word.freqs <- mallet.word.freqs(topic.model)

  topic.model$setAlphaOptimization(20, 50)
  topic.model$train(200)

  doc.topics <- mallet.doc.topics(topic.model, smoothed=FALSE, normalized=FALSE)
  topic.words <- mallet.topic.words(topic.model, smoothed=FALSE, normalized=FALSE)

  expect_equal(sum(topic.words), sum(doc.topics))

  res <- cbind(correct.counts = apply(topic.words, 2, sum), word.freqs)
  expect_equal(res$correct.counts, res$word.freq)

  }
)



test_that(desc="Test that hclust works as expected",{
  skip_on_cran()

  expect_silent(
    sotu.instances <-
      mallet.import(text.array = sotu[["text"]])
  )

  expect_silent(
    topic.model <- MalletLDA(num.topics=5, alpha.sum = 1, beta = 0.1)
  )

  expect_silent(
    topic.model$loadDocuments(sotu.instances)
  )

  expect_silent(
    topic.model$train(20)
  )

  expect_silent(dt <- mallet.doc.topics(topic.model))
  expect_silent(tw <- mallet.topic.words(topic.model))
  expect_silent(
    res <- mallet.topic.hclust(dt, tw)
  )
})
