context("system messages")


test_that("don't capture non-error system messages", {
    ## let errors errors
    expect_error({
        with_logging({
            stop('error message')
        })
    }, 'error message')
    
    ## warnings
    expect_that({
        with_logging({
            message('hello there')
            warning('warning message')
        }, tempfile())
    }, not(gives_warning('warning message')))
    
    ## messages
    expect_that({
        with_logging({
            message('hello there')
        }, tempfile())
    }, not(shows_message('hello there')))
})


### Base messaging functions still go through to their system handlers
test_that("pass-through system messages", {
    ## errors
    expect_error({
        with_logging({
            stop('error message')
        }, catch_system_messages = FALSE)
    }, 'error message')
        
    ## warnings
    expect_warning({
        with_logging({
            message('hello there')
            warning('warning message')
        }, catch_system_messages = FALSE)
    }, 'warning message')
    
    ## messages
    expect_message({
        with_logging({
            message('hello there')
        }, catch_system_messages = FALSE)
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
            }, log_file, threshold = 'WARN')
        })
    })

    ## `message`s are not not captured because they're below 
    expect_identical(readLines(log_file),
                     'Warning: warning message')
})


test_that("logging without an active logger", {
    block <- evaluate_promise({
        info_log('hello there')
        trace_log('detailed output')
        debug_log('debugging message')
        warn_log('warning message')
    })
    
    ## INFO -> message
    expect_equal(block$messages, 'hello there\n')
    
    ## WARN -> warning
    expect_equal(block$warnings, 'warning message\n')
    
    ## ERROR -> stop
    expect_error({
        error_log('error message')
    }, 'error message')
})


test_that("sensible error reporting", {
    expect_error({
        with_logging({
            1+'foo'
        })
    }, '^Error in 1 \\+ "foo"')

    ## `stop` called directly within expression evaluated inside
    ## `with_logging` should appear to have come from `with_logging`,
    ## rather than its internals
    expect_error({
        with_logging(stop('foo'))
    }, '^Error in with_logging\\(stop\\("foo"\\)\\)')
    
    cl <- NULL
    tryCatch({
        with_logging(warning('bar'), catch_system_messages = FALSE)
    }, warning = function(cond) if (is.null(cl)) cl <<- conditionCall(cond) else stop(cl))
    
    expect_match(paste(deparse(cl), collapse='\n'),
                 '^with_logging\\(warning\\("bar"\\), catch_system_messages = FALSE\\)')

    block <- evaluate_promise({
        with_logging(warning('bar'), catch_system_messages = FALSE)
    })
    expect_equal(block$warnings, 'bar')
})
