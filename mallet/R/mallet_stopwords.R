#' @title
#' Return the file path to the mallet stoplists
#'
#' @details
#' Returns the path to the mallet stop word list.
#' See [mallet_supported_stoplists()] for which stoplists that are included.
#'
#' @param language language to return stoplist for. Defaults to engligs ([en]).
#'
#' @export
mallet_stoplist_file_path <- function(language = "en"){
  checkmate::assert_choice(language, choices = mallet_supported_stoplists())
  fp <- system.file("stoplists", package = "mallet")
  file.path(fp, paste0(language,".txt"))
}

#' @export
#' @rdname mallet_stoplist_file_path
mallet.stoplist.file.path <- mallet_stoplist_file_path


#' @title Mallet supported stoplists
#'
#' @details return vector with included stoplists
#'
#' @export
mallet_supported_stoplists <- function(){
  fns <- dir(system.file("stoplists", package = "mallet"))
  fns <- fns[grepl(fns, pattern = "\\.txt$")]
  remove_file_extension(fns)
}

#' @export
#' @rdname mallet_supported_stoplists
mallet.supported.stoplists <- mallet_supported_stoplists

remove_file_extension <- function(x){
  sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
}
