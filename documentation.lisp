;;;; Copyright (c) Eric Diethelm 2018 <ediethelm@yahoo.com>
;;;; This code is licensed under the MIT license.

(in-package :trivial-timer)

(defsection @trivial-timer-manual (:title "Trivial Timer Manual")
  "[![pipeline status](https://gitlab.com/ediethelm/trivial-timer/badges/master/pipeline.svg)](https://gitlab.com/ediethelm/trivial-timer/commits/master)
[![Quicklisp](http://quickdocs.org/badge/trivial-timer.svg)](http://quickdocs.org/trivial-timer/)
[![coverage report](https://gitlab.com/ediethelm/trivial-timer/badges/master/coverage.svg?job=test-coverage)](https://gitlab.com/ediethelm/trivial-timer/-/jobs/artifacts/master/browse?job=test-coverage)"
  (@trivial-timer-description section)
  (@trivial-timer-installing section)
  (@trivial-timer-example section)
  (@trivial-timer-exported section)
  (@trivial-timer-license section)
  (@trivial-timer-contributing section))


(defsection @trivial-timer-description (:title "Description")
  "Trivial Timer allows for easy scheduling of tasks (functions). The default tolerance is +- 10ms as defined by *\\*ms-tolerance\\**.")

(defsection @trivial-timer-installing (:title "Installing trivial-timer")
    "This project is available in the latest [QuickLisp](https://www.quicklisp.org/beta/ \"QuickLisp\") distribution, so installing it is reduced to calling:

```lisp
(ql:quickload :trivial-timer)
```
")

(defsection @trivial-timer-example (:title "Working Example")
  "
```lisp
(in-package :trivial-timer)
(initialize-timer)

(defun timer-callback (registered-time delay)
  (log:info \"I was called by the timer ~a ms after registration. Expected was ~a ms.\"
    (- (get-internal-real-time) registered-time) delay))

(let ((delay 1000))
  (register-timer-call delay #'timer-callback))

=> I was called by the timer 992 ms after registration. Expected was 1000 ms.
```
")

(defsection @trivial-timer-exported (:title "Exported Symbols")
  (*ms-tolerance* variable)
  (initialize-timer function)
  (shutdown-timer function)
  (register-timer-call function)
  (register-timer-recurring-call function))

(defsection @trivial-timer-license (:title "License Information")
  "This library is released under the MIT License. Please refer to the [LICENSE](https://gitlab.com/ediethelm/trivial-timer/blob/master/LICENSE 'License') to get the full licensing text.")

(defsection @trivial-timer-contributing (:title "Contributing to this project")
  "Please refer to the [CONTRIBUTING](https://gitlab.com/ediethelm/trivial-timer/blob/master/CONTRIBUTING.md 'Contributing') document for more information.")


(trivial-utilities:make-doc-updater :trivial-timer @trivial-timer-manual)

