context("mallet-io")

data(sotu)

test_that(desc="Save and load state files",{
  skip_on_cran()

  sotu.instances <-
    mallet.import(id.array = row.names(sotu),
                  text.array = sotu[["text"]],
                  stoplist = mallet_stoplist_file_path("en"),
                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
  topic.model$loadDocuments(sotu.instances)
  topic.model$setAlphaOptimization(10, 10)
  topic.model$train(100)

  old.doctopic <- mallet.doc.topics(topic.model, smoothed=FALSE, normalized=FALSE)
  old.topictype <- mallet.topic.words(topic.model, smoothed=FALSE, normalized=FALSE)
  old.doctopic.prior <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=FALSE)
  old.topictype.prior <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=FALSE)

  state_file <- file.path(tempdir(), "temp_mallet_state.gz")
  wrong_state_file <- file.path(tempdir(), "temp_mallet_state.txt")
  instance_file <- file.path(tempdir(), "instances.txt")

  expect_true(!file.exists(state_file))

  expect_silent(save.mallet.state(topic.model = topic.model, state.file = state_file))
  expect_error(save.mallet.state(topic.model = topic.model, state.file = wrong_state_file))

  expect_true(file.exists(state_file))

  rm(topic.model)

  new.topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
  new.topic.model$loadDocuments(sotu.instances)
  new.doctopic.before.load <- mallet.doc.topics(new.topic.model, smoothed=FALSE, normalized=FALSE)
  new.topictype.before.load <- mallet.topic.words(new.topic.model, smoothed=FALSE, normalized=FALSE)
  new.doctopic.before.load.prior <- mallet.doc.topics(new.topic.model, smoothed=TRUE, normalized=FALSE)
  new.topictype.before.load.prior <- mallet.topic.words(new.topic.model, smoothed=TRUE, normalized=FALSE)

  expect_true(file.exists(state_file))

  expect_silent(load.mallet.state(topic.model = new.topic.model, state.file = state_file))

  skip_on_os("windows")
  expect_equal(unlink(state_file, force = TRUE), 0)
  expect_true(!file.exists(state_file))

  new.doctopic.after.load <- mallet.doc.topics(new.topic.model, smoothed=FALSE, normalized=FALSE)
  new.topictype.after.load <- mallet.topic.words(new.topic.model, smoothed=FALSE, normalized=FALSE)
  new.doctopic.after.load.prior <- mallet.doc.topics(new.topic.model, smoothed=TRUE, normalized=FALSE)
  new.topictype.after.load.prior <- mallet.topic.words(new.topic.model, smoothed=TRUE, normalized=FALSE)

  expect_equal(new.topictype.after.load, old.topictype)
  expect_equal(new.topictype.after.load.prior, old.topictype.prior)
  expect_true(any(new.topictype.after.load != new.topictype.before.load))
  expect_true(any(new.topictype.after.load.prior != new.topictype.before.load.prior))

  expect_equal(144826, sum(old.topictype))
  expect_equal(144826, sum(new.topictype.after.load))
  expect_equal(144826, sum(new.topictype.before.load))


  expect_equal(new.doctopic.after.load, old.doctopic)
  expect_equal(new.doctopic.after.load.prior, old.doctopic.prior)
  expect_true(any(new.doctopic.after.load != new.doctopic.before.load))
  expect_true(any(new.doctopic.after.load.prior != new.doctopic.before.load.prior))

  expect_equal(144826, sum(old.doctopic))
  expect_equal(144826, sum(new.doctopic.before.load))
  expect_equal(144826, sum(new.doctopic.after.load))



  expect_true(!file.exists(instance_file))

  expect_silent(mallet:::save.mallet.instances(sotu.instances, instance_file))

  expect_true(file.exists(instance_file))

  expect_silent(new.sotu.instances <- mallet:::load.mallet.instances(instance_file))

  # Test that the new instances work
  topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
  topic.model$loadDocuments(new.sotu.instances)
  topic.model$train(20)


  expect_equal(unlink(instance_file, force = TRUE), 0)
  expect_true(!file.exists(instance_file))

})


test_that(desc="Save and load topic model",{
  skip_on_cran()

  sotu.instances <-
    mallet.import(id.array = row.names(sotu),
                  text.array = sotu[["text"]],
                  stoplist = mallet_stoplist_file_path("en"),
                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
  topic.model$loadDocuments(sotu.instances)
  topic.model$train(100)

  model_file <- file.path(tempdir(), "temp_mallet.model")
  expect_silent(mallet.topic.model.write(topic.model, model_file))
  expect_silent(read.topic.model <- mallet.topic.model.read(model_file))
  expect_equal(read.topic.model, topic.model)

  dtm1 <- mallet.doc.topics(topic.model)
  dtm2 <- mallet.doc.topics(read.topic.model)
  expect_equal(dtm1, dtm2)

  dtw1 <- mallet.topic.words(topic.model)
  dtw2 <- mallet.topic.words(read.topic.model)
  expect_equal(dtw1, dtw2)
})
