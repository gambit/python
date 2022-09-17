(import (_six python)
        (github.com/gambit/python))

\import csv

(define (read-csv path)
  \f=open(`path)
  \reader=csv.reader(f)
  (let loop ((acc '()))
    (with-exception-catcher
     (lambda (e)
       ;; The exception will be a pair (PyObject* . repr(PyObject*))
       \f.close()
       (if \isinstance(`(car e), StopIteration)
           (reverse acc) ;; Return the result
           (write e)))   ;; Propagate the exception
     ;; Iterate using __next__() until StopIteration is raised
     (lambda () (loop (cons \reader.__next__() acc))))))

(pretty-print (read-csv (path-expand "~~userlib/github.com/gambit/python/@/examples/data.csv")))

;; (("A" "B" "C") ("1" "2" "3"))
