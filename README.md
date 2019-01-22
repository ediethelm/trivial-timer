# Trivial Timer Manual

###### \[in package TRIVIAL-TIMER\]
[![pipeline status](https://gitlab.com/ediethelm/trivial-timer/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-timer/commits/master)

## Description

Trivial Timer allows for easy scheduling of tasks (functions). The default tolerance is +- 10ms as defined by *\*ms-tolerance\**.


## Installing trivial-timer

Since this project is not yet available in the latest [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") distribution, it has to be copied to your local-projects folder:

```bash
cd $HOME/quicklisp/local-projects
git clone https://gitlab.com/ediethelm/trivial-timer.git
```

After the files are copied, we can use [QuickLisp](https://www.quicklisp.org/beta/ "QuickLisp") to load trivial-timer:

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


## License Information

This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-timer/blob/master/LICENSE "License") to get the full licensing text.

## Contributing to this project

Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-timer/blob/master/CONTRIBUTING.md "Contributing") document for more information.