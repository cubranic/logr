#' Outputs a message using the currently active logging outputs
#' 
#' @param x something to put in the log, usually a message
#' @param level the verbosity
#' @param ... optional arguments passed to the low-level functions that 
#'   construct log messages; main usage is to exert control over arguments to 
#'   \code{print.data.frame}, such as \code{right} for alignment and 
#'   \code{row.names}. (See \code{format_message} for details.)
#' @export
logr <- function(x, level, ...) {
  cond <- LoggingMessage(format_message(x, ...), level)
  withRestarts({
      signalCondition(cond)

      ## We'll get here if no restarts were invoked, which can only
      ## happen if the logging is not active. In that case, we'll
      ## simply output to the console
      message(conditionMessage(cond))
  }, output_message = function(log_outputs) {
      lapply(log_outputs, output_message, message = cond)
  }, muffle_logging = function() NULL)
  invisible()
}


#' @export
error <- function(x, ...) {
    logr(x, level = 'ERROR')
}


#' @export
warn <- function(x, ...) {
    logr(x, level = 'WARN')
}


#' @export
info <- function(x, ...) {
    logr(x, level = 'INFO', ...)
}


#' @export
debug <- function(x, ...) {
    logr(x, level = 'DEBUG', ...)
}


#' @export
trace <- function(x, ...) {
    logr(x, level = 'TRACE')
}
