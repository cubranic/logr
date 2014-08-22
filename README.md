_Logr_ implements a logging framework that uses R's existing messaging
functionality, and builds upon it an API that is simple to use.
Because it shares the same underpinnings, _logr_ can capture messages
and warnings generated by `warning` and `message`, making it easy to
adopt even in a mature codebase.

To install the latest development version, run:
`devtools::install_github('cubranic/logr')`.


Example
-------

```r
info_log('Hello there')

if (sample(6, 1) == 1)
    warn_log('Not your lucky day')
```

To start using this package, simply use one of the family of `*_log`
functions to output logging messages at the desired level of
importance (see below for list of all levels). With no other
configuration, these functions will behave as their closest equivalent
in _base_: `info_log` will act exactly like `message` (it can even be
turned off with `suppressMessages`), `warn_log` as `warning` and
`error_log` as `stop`. Messages with their level lower than INFO
(i.e., DEBUG and TRACE) will not be printed.

To exert a finer control, wrap the code where you want to enable
loggging inside a call to `with_logging`. Here you can specify logging
destinations (files, console, or even any writeable connections) and
level thresholds. This is useful to, for instance, output
at most informational messages to the console while recording
fine-level messages in a log file:

```r
with_logging({
    info_log('Hello there')
    
    if (x <- sample(6, 1) == 1)
        warn_log('Not your lucky day')
    trace_log(paste('x:', x))
}, list('/var/tmp/myapp.log', 'DEBUG'), stderr())
```


Message Detail Levels
---------------------

Similar to [_log4j_](http://logging.apache.org/log4j/) and other
logging frameworks, _logr_ provides the following levels for message
detail, from most to least important:

- ERROR: serious, un-recoverable errors that prevent the code from
  continuing with further execution
- WARN: reporting "suspicious" events, for instance, unexpected NAs,
  or that some automatic corrective action has failed (e.g., maximum
  number of retries reached)
- INFO: informing on major steps that software is performing, without
  drowning in too much detail
- DEBUG: detailed steps taken by operations
- TRACE: use for very detailed debugging, like printing local variable
  values
