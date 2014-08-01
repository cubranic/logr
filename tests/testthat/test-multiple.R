context('multiple outputs')

test_that("multiple outputs at different levels", {
    log_file <- tempfile(fileext='.log')

    sink_file <- textConnection('console_out', 'w', local=TRUE)
    on.exit({sink(type='message'); close(sink_file)}, add=TRUE)
    
    with_logging({
        sink(sink_file, type='message')
        info('hello there')
        trace('detailed output')
        debug('debugging message')
    }, c(log_file, 'TRACE'), '', level='DEBUG')
    
    sink(type='message')
    ## INFO, DEBUG, and TRACE messages are sent to the log file
    expect_identical(readLines(log_file),
                     c('hello there',
                       'detailed output',
                       'debugging message'))
    
    ## both INFO and DEBUG messages are sent to the console
    expect_identical(console_out,
                     c('hello there',
                       'debugging message'))
})


test_that("errors and warnings only logging output", {
    log_file <- tempfile(fileext='.log')

    sink_file <- textConnection('console_out', 'w', local=TRUE)
    on.exit({sink(type='message'); close(sink_file)}, add=TRUE)
        
    with_logging({
        sink(sink_file, type='message')
        warn('warning message')
        info('hello there')
        debug('debugging message')
        error('error message')
    }, c(log_file, 'INFO'), c('', 'WARN'))
    
    sink(type='message')
    
    ## only ERROR, WARN, and INFO messages are sent to the log file
    expect_identical(readLines(log_file),
                     c('warning message',
                       'hello there',
                       'error message'))
    
    ## only ERROR and WARN messages are sent to the console
    expect_identical(console_out,
                     c('warning message',
                       'error message'))
})
