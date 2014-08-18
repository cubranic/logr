#' Sends a message to the currently active logging outputs
#' 
#' @param x an object to output in the log, such as a character vector
#' @param level the detail level of the message
#' @param ... optional arguments passed to the low-level functions that 
#'   construct log messages; main usage is to exert control over arguments to 
#'   \code{print.data.frame}, such as \code{right} for alignment and 
#'   \code{row.names}. (See \code{format_message} for details.)
#' @export
add_log <- function(x, level, ...) {
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


#' @describeIn add_log Sends a message at the "ERROR" level
#'
#' @export
error_log <- function(x, ...) {
    add_log(x, level = 'ERROR')
}


#' @describeIn add_log Sends a message at the "WARN" level
#'
#' @export
warn_log <- function(x, ...) {
    add_log(x, level = 'WARN')
}


#' @describeIn add_log Sends a message at the "INFO" level
#'
#' @export
info_log <- function(x, ...) {
    add_log(x, level = 'INFO', ...)
}


#' @describeIn add_log Sends a message at the "DEBUG" level
#'
#' @export
debug_log <- function(x, ...) {
    add_log(x, level = 'DEBUG', ...)
}


#' @describeIn add_log Sends a message at the "TRACE" level
#'
#' @export
trace_log <- function(x, ...) {
    add_log(x, level = 'TRACE')
}
