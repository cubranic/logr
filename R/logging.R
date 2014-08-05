## Executes an expression within the context of specified active
## loggers
#' @export
with_logging <- function(expr, ..., level = NULL) {
    log_outputs <- make_log_output(level, ...)
    on.exit(lapply(log_outputs, close.LogOutput))
    
    withCallingHandlers(expr,
                        LoggingMessage = make_logging_handler(log_outputs),
                        condition = make_system_logging_handler(log_outputs))
}


## evaluate an expression in a context that turns off all logging
#' @export
suppress_logging <- function(expr) {
    withCallingHandlers(expr,
                        LoggingMessage = function(cond) invokeRestart('muffle_logging'))
}


make_log_output <- function(default_level, ...) {
    destinations <- list(...)
    if (length(destinations) == 0) {
        destinations <- list(stderr())
    }
    Reduce(function(outputs, d) {
        dd <- if (length(d) == 1) {
            if (is.null(default_level)) {
                LogOutput(d)
            } else {
                LogOutput(d, default_level)
            }
        } else if (length(d) == 2) {
            d <- as.list(d)
            LogOutput(d[[1]], d[[2]])
        } else stop('Each output destination has to be a file name, connection, or a pair of (destination, detail_level)')
        
        if (is.null(dd$level)) {
            close.LogOutput(dd)
            outputs
        } else {
            c(outputs, list(dd))
        }
    }, destinations, init = list())
}
