context("log to file")

file_helper <- function(log_file = tempfile(fileext='.log'), ..., expected) {
    sink_file <- textConnection('console_out', 'w', local=TRUE)
    on.exit({sink(type='message'); close(sink_file)}, add=TRUE)
    
    with_logging({
        sink(sink_file, type='message')
        info('hello there')
        trace('detailed output')
        debug('debugging message')
    }, log_file, ...)
    
    sink(type='message')

    ## only the expected messages are in the log file
    expect_identical(readLines(log_file[[1]]), # in case caller passed `c(dest, level)`
                     expected)
    
    ## nothing is sent to the console
    expect_identical(console_out, character(0))
}


test_that('file-only output', {
    file_helper(expected = 'hello there')
})


test_that('file-only at debug level', {
    file_helper(expected = c('hello there',
                             'debugging message'),
                level = 'DEBUG')
})


test_that('file-only at trace level', {
    file_helper(c(tempfile(), 'TRACE'),
                expected = c('hello there',
                             'detailed output',
                             'debugging message'))
})
