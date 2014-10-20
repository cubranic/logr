LogOutput <- function(destination=stderr(), threshold) {
    connection <- if (identical(destination, '')) {
        stderr()
    } else if (is.character(destination) &&
               length(destination) == 1L) {
        file(destination)
    } else if (inherits(destination, 'connection')) {
        destination
    } else stop('unknown logging destination')
    
    if (!isOpen(connection)) open(connection, 'wt')
    
    structure(list(connection = connection,
                   threshold = logging_level(threshold)),
              class = 'LogOutput')
}


logging_level <- function(level) {
    if (is.null(level)) return(level)
    
    ll <- ordered(toupper(level),
                  levels = c('ERROR', 'WARN', 'INFO', 'DEBUG', 'TRACE'))
    if (is.na(ll)) stop('unknown logging level "', level, '"')

    ll
}


## Prints the message to the given output if the message's detail
## level is lower than the output's threshold.
##
## @param log_output
## @param message
## @return None (invisible \code{NULL})
output_message <- function(log_output, message) {
    if (message$level <= log_output$threshold)
        cat(conditionMessage(message), file = log_output$connection, sep='')
}


## S3 method for class 'LogOutput'
##
## Closes the output's connection iff it's an instance of `file`.
## (I.e., it won't close a terminal connection, such as that to
## `stderr()`.)
close.LogOutput <- function(target, ...) {
    if (inherits(target$connection, 'file')) {
      close(target$connection, ...)
    }       
}
