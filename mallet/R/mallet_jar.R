#' @title
#' Return the mallet jar filename(s)
#'
#' @details
#' Mallet is implemented as a jar-file in the mallet R package.
#' This function returns the file name and file path for that file(s)
#'
#' @param full.names a logical value.
#' If TRUE, the directory path is prepended to the file names to give
#' a relative file path. If FALSE, the file name(s) (rather than paths)
#' are returned.
#'
#' @export
mallet_jar <- function(full.names = FALSE){
  fp <- system.file("java", package = "mallet")
  dir(fp, full.names = full.names)
}

#' @rdname mallet_jar
#' @export
mallet.jar <- mallet_jar
