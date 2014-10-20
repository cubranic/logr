context("log to console")

console_helper <- function(..., expected) {
    sink_file <- textConnection('console_out', 'w', local=TRUE)
    on.exit({sink(type='message'); close(sink_file)}, add=TRUE)
    
    with_logging({
        sink(sink_file, type='message')
        info_log('hello there')
        trace_log('detailed output')
        debug_log('debugging message')
    }, ...)
    
    sink(type='message')
    
    ## only INFO messages are sent to the console
    expect_identical(console_out,
                     expected)
}


test_that('default', {
    console_helper(expected = 'hello there')
})


test_that('default output with debug as the default threshold', {
    console_helper(expected = c('hello there',
                                'debugging message'),
                   threshold = 'DEBUG')
})


test_that('NULL threshold', {
    console_helper(list('', NULL),
                   expected = character(0))
})


test_that('NULL default threshold', {
    console_helper(threshold = NULL,
                   expected = character(0))
})
