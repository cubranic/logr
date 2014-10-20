## Constructs a condition handler that receives and logs conditions
## signalled by `logr`.
##
## @param outputs active destinations for logging messages (a
##        vector of LogOutputs)
## @return a function that can be used as an argument to
##        \code{withCallingHandlers}
make_logging_handler <- function(outputs = list(LogOutput(stderr()))) {
    function(cond) {
        invokeRestart('output_message', outputs)
    }
}


## Constructs a condition handler that receives *and logs* conditions
## signalled by `stop`, `warning`, and `message`.
##
## The handler works by converting the condition to an instance of
## LoggingMessage at level ERROR, WARN, or INFO, respectively. The
## logging message is output only to logging outputs *other than*
## `stderr()`. The handler does not invoke any restarts, so the
## handling will pass on to any other active handlers up the calling
## chain. It is assumed that one of those will output a message to the
## console, if the user hasn't suppressed it.
##
## @param log_outputs active destinations for logging messages (a
##        vector of LogOutputs)
## @param catch_system_messages whether `warning` and `message` should
##        be let through for further processing by upstream active
##        handlers
make_system_logging_handler <- function(log_outputs, catch_system_messages) {
    internal_call <- sys.call(-1)
    
    ## we won't output to stderr since it's assumed there is a system
    ## handler to deal with it
    non_console_outputs <- log_outputs[stderr() != lapply(log_outputs, `[[`,
                                                          'connection')]
    ## mapping of logging levels to system conditions
    system_conditions <- c(ERROR='error', WARN='warning', INFO='message')
    
    function(cond) {
        is_system_condition <- is.element(system_conditions,
                                          class(cond))
        if (any(is_system_condition)) {
            message_level <- names(system_conditions)[which(is_system_condition)]
            logging_message <- LoggingMessage(format_message(cond),
                                              message_level,
                                              conditionCall(cond))
            if (inherits(cond, 'error') || !catch_system_messages) {
                lapply(non_console_outputs,
                       output_message, message = logging_message)
                
                if (isTRUE(all.equal(conditionCall(cond), internal_call))) {
                    invokeRestart('resignal_with_call', cond)
                }
            }
            else {
                lapply(log_outputs, output_message, message = logging_message)
                
                if (identical(message_level, 'WARN')) {
                    invokeRestart('muffleWarning')
                }
                else if (identical(message_level, 'INFO')) {
                    invokeRestart('muffleMessage')
                }
            }
        }
    }
}
