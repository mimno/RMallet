context("mallet-deprecated")

data(sotu)

test_that(desc="mallet.read.dir",{
  skip_on_cran()
  test_folder <- file.path(tempdir(), "test_mallet_read_dir")
  dir.create(test_folder, showWarnings = FALSE)

  expect_warning(mallet::mallet.read.dir(test_folder), regexp = "deprecated")

})


