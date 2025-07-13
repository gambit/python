#!/usr/bin/env gsi-script

(define-library (github.com/gambit/python demo)

  (import (..)) ;; relative import of python (preserves the version)
  (import (except (gambit) six.infix)) ;; for lambda, this-source-file, etc
  (import (_six python)) ;; for \... syntax

  (begin

    (define year 2025)

    (println "Calendar for " year ":")

    \import calendar                        

    (print \calendar.calendar(`year))

    (println "Demo source code: " (this-source-file))))
