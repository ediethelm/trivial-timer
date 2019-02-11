# Trivial Timer Manual

###### \[in package TRIVIAL-TIMER\]
[![pipeline status](https://gitlab.com/ediethelm/trivial-timer/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-timer/commits/master)
[![Quicklisp](http://quickdocs.org/badge/trivial-timer.svg)](http://quickdocs.org/trivial-timer/)

## Description

Trivial Timer allows for easy scheduling of tasks (functions). The default tolerance is +- 10ms as defined by *\*ms-tolerance\**.


## Installing trivial-timer

This project is available in the latest [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") distribution, so installing it is reduced to calling:

```lisp
(ql:quickload :trivial-timer)
```


## Working Example

```lisp
(in-package :trivial-timer)
(initialize-timer)

(defun timer-callback (registered-time delay)
  (log:info "I was called by the timer ~a ms after registration. Expected was ~a ms."
    (- (get-internal-real-time) registered-time) delay))

(let ((delay 1000))
  (register-timer-call delay #'timer-callback))

=> I was called by the timer 992 ms after registration. Expected was 1000 ms.
```


## Exported Symbols

- [variable] *MS-TOLERANCE* 10

    Time tolerance (default +/- 10ms)

- [function] INITIALIZE-TIMER 

    Initialization of trivial-timer. This **MUST** be called before any other function from this library.

- [function] SHUTDOWN-TIMER 

    Shutdown the timer. No further calls can be registered. Atention: Stopping is an asynchronous request, meaning that some registered call might still be executed after calling *shutdown-timer*

- [function] REGISTER-TIMER-CALL OFFSET CALL

    Register a function *call* to be executed in *offset* milliseconds from now.

- [function] REGISTER-TIMER-RECURRING-CALL OFFSET CALL

    Register a function *call* to be (recurrently) executed every *offset* milliseconds.

## License Information

This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-timer/blob/master/LICENSE "License") to get the full licensing text.

## Contributing to this project

Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-timer/blob/master/CONTRIBUTING.md "Contributing") document for more information.