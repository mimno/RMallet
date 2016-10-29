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

old.doctopic <- mallet.doc.topics(topic.model, smoothed=FALSE, normalized=FALSE)
old.topictype <- mallet.topic.words(topic.model, smoothed=FALSE, normalized=FALSE)

state_file <- file.path(tempdir(), "temp_mallet_state.gz")
instance_file <- file.path(tempdir(), "instances.txt")

test_that(desc="save.mallet",{
  
  expect_true(!file.exists(state_file))
  
  expect_silent(save.mallet.state(topic.model = topic.model, state.file = state_file))
  
  expect_true(file.exists(state_file))
  
})

rm(topic.model)

test_that(desc="load.mallet",{
  
  new.topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
  new.topic.model$loadDocuments(sotu.instances)
  new.doctopic.before.load <- mallet.doc.topics(new.topic.model, smoothed=FALSE, normalized=FALSE)
  new.topictype.before.load <- mallet.topic.words(new.topic.model, smoothed=FALSE, normalized=FALSE)
  
  expect_true(file.exists(state_file))

  expect_silent(load.mallet.state(topic.model = new.topic.model, state.file = state_file))
  
  expect_equal(unlink(state_file), 0)
  expect_true(!file.exists(state_file))
  
  new.doctopic.after.load <- mallet.doc.topics(new.topic.model, smoothed=FALSE, normalized=FALSE)
  new.topictype.after.load <- mallet.topic.words(new.topic.model, smoothed=FALSE, normalized=FALSE)

  expect_equal(new.topictype.after.load, old.topictype)
  expect_true(any(new.topictype.after.load != new.doctopic.before.load))
  expect_equal(680947, sum(old.topictype))
  expect_equal(680947, sum(new.topictype.after.load))
  expect_equal(680947, sum(new.topictype.before.load))
  
  skip("Currently a bug in mallet?")
  expect_equal(new.doctopic.after.load, old.doctopic)
  expect_true(any(new.doctopic.after.load != new.doctopic.before.load))
  expect_equal(680947, sum(old.doctopic))
  expect_equal(680947, sum(new.doctopic.before.load))
  expect_equal(680947, sum(new.doctopic.after.load))
  
})



test_that(desc="mallet.instances io",{
  
  expect_true(!file.exists(instance_file))
  
  expect_silent(mallet:::save.mallet.instances(sotu.instances, instance_file))
  
  expect_true(file.exists(instance_file))
  
  expect_silent(new.sotu.instances <- mallet:::load.mallet.instances(instance_file))
  
  # Test that the new instances work
  topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
  topic.model$loadDocuments(new.sotu.instances)
  topic.model$train(20)
})
