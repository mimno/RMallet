context("mallet-deprecated")

data(sotu)

test_that(desc="mallet.read.dir",{
  skip_on_cran()
  directory <- system.file("stoplists", package = "mallet")

  expect_warning(mallet::mallet.read.dir(directory), regexp = "deprecated")

})


