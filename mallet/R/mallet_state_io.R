#' Save a Mallet state to file
#' 
#' @description 
#' This function writes a current sampling state of mallet to file. 
#' The state contain hyperparameters \eqn{\alpha} and \eqn{\beta} together with topic indicators.
#' 
#' The state file can be read into R using the function 
#' 
#' @param topic.model a mallet \code{topic.model} object
#' @param state.file File path to store the mallet state file to.
#' 
#' @export
save.mallet.state <- function(topic.model, state.file){
  checkmate::assert_path_for_output(state.file)
  checkmate::assert_class(topic.model, "jobjRef")
  checkmate::assert_string(utils::capture.output(print(topic.model)), "Java-Object{cc.mallet.topics.RTopicModel@[a-zA-z0-9]+}")

  topic.model$writeState(state.file)
}

#' Load a Mallet state into Mallet
#' 
#' @description 
#' This reads writes a current sampling state of mallet to file. The state contain 
#' hyperparameters \eqn{\alpha} and \eqn{\beta} together with topic indicators.
#' 
#' @param topic.model A Mallet \code{topic.model} object
#' @param state.file File path to store the mallet state file to.
#' 
#' @export
load.mallet.state <- function(topic.model, state.file){
  checkmate::assert_file_exists(state.file)
  checkmate::assert_class(topic.model, "jobjRef")
  checkmate::assert_string(utils::capture.output(print(topic.model)), "Java-Object{cc.mallet.topics.RTopicModel@[a-zA-z0-9]+}")
  
  jFile <- rJava::.jnew("java/io/File", state.file)
  topic.model$initializeFromState(jFile)
}

