context("log to file")

file_helper <- function(log_file = tempfile(fileext='.log'), ..., expected) {
    sink_file <- textConnection('console_out', 'w', local=TRUE)
    on.exit({sink(type='message'); close(sink_file)}, add=TRUE)
    
    with_logging({
        sink(sink_file, type='message')
        info_log('hello there')
        trace_log('detailed output')
        debug_log('debugging message')
    }, log_file, ...)
    
    sink(type='message')

    ## only the expected messages are in the log file
    expect_identical(readLines(log_file[[1]]), # in case caller passed `c(dest, threshold)`
                     expected)
    
    ## nothing is sent to the console
    expect_identical(console_out, character(0))
}


test_that('file-only output', {
    file_helper(expected = 'hello there')
})


test_that('file-only at debug threshold', {
    file_helper(expected = c('hello there',
                             'debugging message'),
                threshold = 'DEBUG')
})


test_that('file-only at trace threshold', {
    file_helper(c(tempfile(), 'TRACE'),
                expected = c('hello there',
                             'detailed output',
                             'debugging message'))
})


test_that('file-only at NULL threshold', {
    file_helper(list(tempfile(), NULL),
                expected = character(0))
})


test_that('file-only at default NULL threshold', {
    file_helper(threshold = NULL,
                expected = character(0))
})


test_that('smarter message wrapping', {
    log_file <- tempfile(fileext='.log')
    
    ## no wrapping happens with short text
    with_logging({
        info_log('foo   bar bla ')
        debug_log('ignored')
    }, log_file)
     
    expect_identical(readLines(log_file),
                     'foo   bar bla ')

    ## messages with long lines will by default be wrapped
    opt <- options(width=14)
    on.exit(options(opt))
    with_logging({
        info_log('foo   bar bla ')
        debug_log('ignored')
        warn_log('foo   bar bla ', wrap = FALSE) # choose no-wrap
    }, log_file)
    
    expect_identical(readLines(log_file),
                     c('foo bar bla',
                       'foo   bar bla '))
})
