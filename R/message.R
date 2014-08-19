LoggingMessage <- function(message, level = 'INFO', call = NULL) {
    class <- c('LoggingMessage', 'condition')
    structure(list(message = as.character(message),
                   level = logging_level(level),
                   call = call),
              class = class)
}


#' Construct logging message text
#'
#' Generic for constructing the text of logging messages.
#'
#' These functions provide a convenient way to format logging messages
#' based on the object being logged. For instance, data frames are
#' formatted using \code{\link[base]{print.data.frame}}, while text
#' objects can optionally be formatted using
#' \code{\link[base]{strwrap}}. When formatting is delegated to these
#' methods, the caller can control them by passing additional
#' arguments, such as \code{width} for \code{strwrap}.
#'
#' @param x the object to put in the log
#' @param appendLF (logical) should the message text have a newline
#'                 appended at each line
#' @param wrap (logical) should the message text first be reformatted
#'             using `strwrap`
#' @param ... additional arguments passed through to specific methods (see below)
#'
#' @keywords internal
#' @export
format_message <- function(x, ...) {
    UseMethod('format_message')
}


#' @describeIn format_message coerce the argument to character and
#' concatenate its elements
#'
#' @keywords internal
#' @export
format_message.default <- function(x, appendLF = TRUE, ...) {
    message <- paste(as.character(x), collapse = '')
    format_message.character(message, appendLF = appendLF, wrap = FALSE)
}


#' @describeIn format_message log the character vector optionally
#' formatting it using \code{\link[base]{strwrap}}
#'
#' @keywords internal
#' @export
format_message.character <- function(x, appendLF = TRUE, wrap = any(nchar(x) > 0.9 * getOption('width')), ...) {
    if (wrap) {
        x <- strwrap(x, ...)
    }
    
    if (appendLF) {
        paste0(x, '\n')
    } else x
}


#' @describeIn format_message log the data frame using
#' \code{\link[base]{print.data.frame}} to construct in textual
#' representation
#'
#' @keywords internal
#' @export
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
    format_message.character(paste('Message:', conditionMessage(x)),
                             appendLF = FALSE, wrap = FALSE)
}


format_message.warning <- function(x, ...) {
    format_message.character(paste('Warning:', conditionMessage(x)),
                             wrap = FALSE)
}


format_message.error <- function(x, ...) {
    format_message.character(paste('Error:', conditionMessage(x)),
                             wrap = FALSE)
}
