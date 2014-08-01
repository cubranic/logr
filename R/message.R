LoggingMessage <- function(message, level = 'INFO', call = NULL) {
    class <- c('LoggingMessage', 'condition')
    structure(list(message = as.character(message),
                   level = logging_level(level),
                   call = call),
              class = class)
}


## Generic for generating the text of logging messages from its
## argument
##
## @param x the object to put in the log
## @param ... further arguments to specific methods
format_message <- function(x, ...) {
    UseMethod('format_message')
}


## The default `format_message` method simply coerces its argument to
## character and pastes it together with no separator
##
## @param x the object to put in the log
## @param appendLF (logical) should the message text have a newline
##                 appended at each line
## @param ... further arguments to other methods that are ignored in
##            this function
format_message.default <- function(x, appendLF = TRUE, ...) {
    message <- paste(as.character(x), collapse = '')
    format_message.character(message, appendLF = appendLF, wrap = FALSE)
}


## The `format_message` method for character vectors optionally formats
## it by wrapping long lines using `base::strwrap`
##
## @param x the character vector object to put in the log
## @param appendLF (logical) should the message text have a newline
##                 appended at each line
## @param ... optional arguments passed through to `strwrap`
format_message.character <- function(x, appendLF = TRUE, wrap = TRUE, ...) {
    if (wrap) {
        x <- strwrap(x, ...)
    }
    
    if (appendLF) {
        paste0(x, '\n')
    } else x
}


## The `format_message` method for data frames converts the object to its
## textual representation using `base::print.data.frame`
##
## @param x the data frame object to put in the log
## @param ... optional arguments passed through to `print.data.frame`
format_message.data.frame <- function(x, ...) {
    message <- NULL
    file <- textConnection('message', 'w', local = TRUE)
    sink(file)
    on.exit({
        sink()
        close(file)
    })
    print(x, ...)
    format_message.default(paste(message, collapse='\n'),
                        appendLF = TRUE)
}


format_message.message <- function(x, ...) {
    format_message.character(paste('Message:', conditionMessage(x)))
}


format_message.warning <- function(x, ...) {
    format_message.character(paste('Warning:', conditionMessage(x)))
}


format_message.error <- function(x, ...) {
    format_message.character(paste('Error:', conditionMessage(x)))
}
