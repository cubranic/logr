context("system messages")


### Base messaging functions still go through to their system handlers
test_that("pass-through system messages", {
    ## errors
    expect_error({
        with_logging({
            stop('error message')
        })
    }, 'error message')
        
    ## warnings
    expect_warning({
        with_logging({
            message('hello there')
            warning('warning message')
        })
    }, 'warning message')
    
    ## messages
    expect_message({
        with_logging({
            message('hello there')
        })
    }, 'hello there')
})


### Base messaging functions are *at the same time* also captured in
### the log file if one is active
test_that("logging system messages", {
    log_file <- tempfile()
    
    try({
        with_logging({
            stop('error message')
        }, log_file)
    }, silent = TRUE)
    
    ## the log file includes errors sent by `stop`
    expect_identical(readLines(log_file),
                     c('Error: error message'))

    suppressMessages({
        suppressWarnings({
            with_logging({
                message('hello there')
                warning('warning message')
            }, log_file)
        })})
    
    ## both `warning`s and `message`s are captured in the log file
    expect_identical(readLines(log_file),
                     c('Message: hello there',
                       'Warning: warning message'))
    
    suppressMessages({
        suppressWarnings({
            with_logging({
                message('hello there')
                warning('warning message')
            }, log_file, level = 'WARN')
        })
    })

    ## `message`s are not not captured because they're below 
    expect_identical(readLines(log_file),
                     'Warning: warning message')
})
