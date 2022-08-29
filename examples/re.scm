(import (_six python))
(import (github.com/udem-dlteam python))
\import re

(define (re.Pattern-converter obj)
  (lambda (attr . args)
    (case attr
      ((pattern) \(`obj).pattern)
      ((match) \(`obj).match(`(car args)))
      ((search) \(`obj).search(`(car args))))))

(PyObject*-register-converter "re.Pattern" re.Pattern-converter)

(define (compile pattern)
  \re.compile(`pattern))

(define pat (compile "s....e"))

(define m (pat 'search "(sch3me)"))

(println m)
;; #<PyObject* #2 <re.Match object; span=(1, 7), match='sch3me'>>
