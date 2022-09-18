(import (_six python)
        (github.com/gambit/python))

\import csv

(define python-open \open)
(define csv.reader \csv.reader)

(define (read-csv path)
  (let* ((f (python-open path))
         (reader (csv.reader f))
         (acc '()))
    (with-exception-catcher
     (lambda (e)
       ;; The exception will be a pair (PyObject* . repr(PyObject*))
       \(`f).close()
       (if \isinstance(`(car e), StopIteration)
           (reverse acc) ;; Return the result
           (raise e)))   ;; Propagate the exception
     (lambda ()
       (let loop ()
         ;; Iterate using __next__() until StopIteration is raised
         (set! acc (cons \(`reader).__next__() acc))
         (loop))))))

(path-expand "data.csv" (path-directory (this-source-file)))

;; (("A" "B" "C") ("1" "2" "3"))
