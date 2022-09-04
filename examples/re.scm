(import (_six python)
        (github.com/gambit/python))

\import re

(define (re-search pattern string)
  \re.search(`pattern, `string))

(define (re-group match i)
  \(`match).group(`i))

(define m (re-search "s....e" "(sch3me)"))

(pp (re-group m 0)) ;; prints "sch3me"
