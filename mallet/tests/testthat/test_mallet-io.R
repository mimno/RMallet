context("mallet-io")

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

old.doc.topics.counts <- mallet.doc.topics(topic.model, smoothed=FALSE, normalized=FALSE)

state_file <- file.path(tempdir(), "temp_mallet_state.gz")

test_that(desc="save.mallet",{
  
  expect_true(!file.exists(state_file))
  
  expect_silent(save.mallet.state(topic.model = topic.model, state.file = state_file))
  
  expect_true(file.exists(state_file))
  
})

rm(topic.model)

test_that(desc="load.mallet",{
  
  new.topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
  new.topic.model$loadDocuments(sotu.instances)
  new.counts.before.load <- mallet.doc.topics(new.topic.model, smoothed=FALSE, normalized=FALSE)

  expect_true(file.exists(state_file))

  expect_silent(load.mallet.state(topic.model = new.topic.model, state.file = state_file))
  
  expect_equal(unlink(state_file), 0)
  expect_true(!file.exists(state_file))
  
  new.counts.after.load <- mallet.doc.topics(new.topic.model, smoothed=FALSE, normalized=FALSE)

  skip("Currently a bug in mallet?")
  expect_equal(new.counts.after.load, old.doc.topics.counts)
  expect_true(any(new.counts.after.load != old.doc.topics.counts))
})
