context("mallet-examples")

test_that(desc="Test that all examples runs correctly",{
  skip_on_cran()

  # Read in sotu example data
  data(sotu)
  sotu.instances <-
    mallet.import(id.array = row.names(sotu),
                  text.array = sotu[["text"]],
                  stoplist = mallet_stoplist_file_path("en"),
                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

  # Create topic model
  topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
  topic.model$loadDocuments(sotu.instances)
  word_freqs <- mallet.word.freqs(topic.model)

  # Train topic model
  topic.model$train(200)

  # Extract results
  doc_topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
  topic_words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
  top_words <- mallet.top.words(topic.model, word.weights = topic_words[2,], num.top.words = 5)

  # Extract subcorpus topic word matrix
  post1975_topic_words <- mallet.subset.topic.words(topic.model, sotu[["year"]] > 1975)

  # Create hiearchical clusters of topics
  topic_labels <- mallet.topic.labels(topic.model)
  plot(mallet.topic.hclust(doc.topics, topic.words, balance = 0.3), labels=topic_labels)

})
