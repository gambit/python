;;;============================================================================

;;; File: "test-for-leaks.scm"

;;; Copyright (c) 2022 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(import python)   (##add-exit-job! cleanup-fpc)

;;;----------------------------------------------------------------------------

(define (get-rss) ;; returns process' RSS in bytes
  (call-with-input-string
      (cdr (shell-command
            (string-append "ps -o rss -p " (number->string (##os-getpid)))
            #t))
    (lambda (port)
      (read-line port) ;; skip first line
      (* 1024 (read port)))))

(define (sweep)
  (let loop ((i 10000000) (r #f))
    (if (> i 0)
        (loop (- i 1) (cons i i)))))

(define (bytes-leaked thunk)
  (define chunk 1000)
  (let* ((start-time (time->seconds (current-time)))
         (start-rss (get-rss)))
    (let loop1 ((iters 0))
      (##gc)
      (let ((end-time (time->seconds (current-time))))
        (if (< (- end-time start-time) 5)
            (let loop2 ((i chunk))
              (if (> i 0)
                  (begin
                    (thunk)
                    (loop2 (- i 1)))
                  (loop1 (+ iters chunk))))
            (let* ((end-rss (get-rss))
                   (leak (- end-rss start-rss)))
;;              (pp (list leak: (inexact (/ leak iters))))
              (inexact (/ leak iters))))))))

(define (test name thunk)
  (let ((n (bytes-leaked thunk)))
    (if (>= n 8)
        (println "***** " n " bytes leaked by " name)
        (println "----- no bytes leaked by " name))))

(sweep)

;;;----------------------------------------------------------------------------

(let* ((f (lambda () (void->PyObject*/None (void))))
       (obj (f)))
  (test 'void->PyObject*/None f)
  (test 'PyObject*/None->void (lambda () (PyObject*/None->void obj))))
  
(let* ((f (lambda () (boolean->PyObject*/bool #f)))
       (obj (f)))
  (test 'boolean->PyObject*/bool f)
  (test 'PyObject*/bool->boolean (lambda () (PyObject*/bool->boolean obj))))

(let* ((f (lambda () (boolean->PyObject*/bool #t)))
       (obj (f)))
  (test 'boolean->PyObject*/bool f)
  (test 'PyObject*/bool->boolean (lambda () (PyObject*/bool->boolean obj))))

(let* ((f (lambda () (exact-integer->PyObject*/int 123456)))
       (obj (f)))
  (test 'exact-integer->PyObject*/int f)
  (test 'PyObject*/int->exact-integer (lambda () (PyObject*/int->exact-integer obj))))

(let* ((f (lambda () (exact-integer->PyObject*/int 12345678901234567890)))
       (obj (f)))
  (test 'exact-integer->PyObject*/int f)
  (test 'PyObject*/int->exact-integer (lambda () (PyObject*/int->exact-integer obj))))

(let* ((f (lambda () (flonum->PyObject*/float 1.2)))
       (obj (f)))
  (test 'flonum->PyObject*/float f)
  (test 'PyObject*/float->flonum (lambda () (PyObject*/float->flonum obj))))

(let* ((f (lambda () (flonums->PyObject*/complex 1.2 3.4)))
       (obj (f)))
  (test 'flonums->PyObject*/complex f)
  (test 'PyObject*/complex->cpxnum (lambda () (PyObject*/complex->cpxnum obj))))

(let* ((num (exact-integer->PyObject*/int 123456))
       (den (exact-integer->PyObject*/int 654321))
       (f (lambda () (ints->PyObject*/Fraction num den)))
       (obj (f)))
  (test 'ints->PyObject*/Fraction f)
  (test 'PyObject*/Fraction->ratnum (lambda () (PyObject*/Fraction->ratnum obj))))

(let* ((f (lambda () (string->PyObject*/str "abc")))
       (obj (f)))
  (test 'string->PyObject*/str f)
  (test 'PyObject*/str->string (lambda () (PyObject*/str->string obj))))

(let* ((f (lambda () (object->SchemeObject (list 1 2 3))))
       (obj (f)))
  (test 'object->SchemeObject f)
  (test 'SchemeObject->object (lambda () (SchemeObject->object obj))))

(let* ((f (lambda () (procedure->SchemeProcedure sqrt)))
       (obj (f)))
  (test 'procedure->SchemeProcedure f))

(let* ((elem0 (exact-integer->PyObject*/int 0))
       (elem1 (exact-integer->PyObject*/int 1))
       (elem2 (exact-integer->PyObject*/int 2))
       (x (list elem0 elem1 elem2))
       (f (lambda () (list->PyObject*/list x)))
       (obj (f)))
  (test 'list->PyObject*/list f)
  (test 'PyObject*/list->list (lambda () (PyObject*/list->list obj))))

(let* ((elem0 (exact-integer->PyObject*/int 0))
       (elem1 (exact-integer->PyObject*/int 1))
       (elem2 (exact-integer->PyObject*/int 2))
       (x (vector elem0 elem1 elem2))
       (f (lambda () (vector->PyObject*/list x)))
       (obj (f)))
  (test 'vector->PyObject*/list f)
  (test 'PyObject*/list->vector (lambda () (PyObject*/list->vector obj))))

(let* ((elem0 (exact-integer->PyObject*/int 0))
       (elem1 (exact-integer->PyObject*/int 1))
       (elem2 (exact-integer->PyObject*/int 2))
       (x (vector elem0 elem1 elem2))
       (f (lambda () (vector->PyObject*/tuple x)))
       (obj (f)))
  (test 'vector->PyObject*/tuple f)
  (test 'PyObject*/tuple->vector (lambda () (PyObject*/tuple->vector obj))))

(let* ((f (lambda () (u8vector->PyObject*/bytes '#u8(1 2 3))))
       (obj (f)))
  (test 'u8vector->PyObject*/bytes f)
  (test 'PyObject*/bytes->u8vector (lambda () (PyObject*/bytes->u8vector obj))))

(let* ((f (lambda () (s8vector->PyObject*/bytes '#s8(1 2 3))))
       (obj (f)))
  (test 's8vector->PyObject*/bytes f))

(let* ((f (lambda () (u8vector->PyObject*/bytearray '#u8(1 2 3))))
       (obj (f)))
  (test 'u8vector->PyObject*/bytearray f)
  (test 'PyObject*/bytearray->u8vector (lambda () (PyObject*/bytearray->u8vector obj))))

;;;============================================================================
