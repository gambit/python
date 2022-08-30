;;;============================================================================

;;; File: "test.scm"

;;; Copyright (c) 2022 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(import (_six python))
(import (github.com/udem-dlteam python))
(import _test)

;;;----------------------------------------------------------------------------

;; test basic Python to Scheme conversions

(test-equal (void) \None)

(test-equal #f \False)
(test-equal #t \True)

(test-equal 1 \1)
(test-equal 0 \0)
(test-equal -1 \-1)
(test-equal 123456 \123456) ;; fixnum
(test-equal 12345678901234567890 \12345678901234567890) ;; bignum
(test-equal 1.0 \1.0)
(test-equal 0.0 \0.0)
(test-equal -0.0 \-0.0)
(test-equal -1.0 \-1.0)
(test-equal 1.234e56 \1.234e56)
(test-equal 1e308 \1e308)
(test-equal +inf.0 \float("+inf"))
(test-equal -inf.0 \float("-inf"))
(test-assert (nan? \float("nan")))
(test-equal 1.2+3.4i \complex("1.2+3.4j"))
(test-equal -1.2-3.4i \complex("-1.2-3.4j"))
(test-equal -2/3 \__import__("fractions").Fraction(-2, 3))
(test-equal 2/3 \__import__("fractions").Fraction(2, 3))
(test-equal -2/3 \__import__("fractions").Fraction(-2, 3))

(test-equal '() \list())
(test-equal '(0) \list(range(1)))
(test-equal '(0 1 2 3) \list(range(4)))

(test-equal '#() \tuple())
(test-equal '#(0) \tuple(range(1)))
(test-equal '#(0 1 2 3) \tuple(range(4)))

(test-equal '#u8() \bytes())
(test-equal '#u8(0) \bytes(range(1)))
(test-equal '#u8(0 1 2 3) \bytes(range(4)))

(test-equal "" \"")
(test-equal "a" \"a")
(test-equal "A B" \"A B")

(test-equal (list->table '()) \dict())
(test-equal (list->table '(("a" . 11))) \dict([tuple(["a", 11])]))
(let ((t \dict([tuple(["a", 11]), tuple(["b", 22])])))
  (test-equal 2 (table-length t))
  (test-equal 11 (table-ref t "a" #f))
  (test-equal 22 (table-ref t "b" #f)))

;;;----------------------------------------------------------------------------

;; test basic Scheme to Python conversions

(test-assert \None==`(void))

(test-assert \False==`#f)
(test-assert \True==`#t)

(test-assert \1==`1)
(test-assert \0==`0)
(test-assert \-1==`-1)
(test-assert \123456==`123456) ;; fixnum
(test-assert \12345678901234567890==`12345678901234567890) ;; bignum

(test-assert \1.0==`1.0)
(test-assert \0.0==`0.0)
(test-assert \-0.0==`-0.0)
(test-assert \-1.0==`-1.0)
(test-assert \1.234e56==`1.234e56)
(test-assert \1e308==`1e308)
(test-assert \float("+inf")==`+inf.0)
(test-assert \float("-inf")==`-inf.0)
(test-assert (nan? \float("nan")))
(test-assert \complex("1.2+3.4j")==`1.2+3.4i)
(test-assert \complex("-1.2-3.4j")==`-1.2-3.4i)
(test-assert \__import__("fractions").Fraction(-2, 3)==`-2/3)
(test-assert \__import__("fractions").Fraction(2, 3)==`2/3)
(test-assert \__import__("fractions").Fraction(-2, 3)==`-2/3)

(test-assert \list()==`'())
(test-assert \list(range(1))==`'(0))
(test-assert \list(range(4))==`'(0 1 2 3))

(test-assert \tuple()==`'#())
(test-assert \tuple(range(1))==`'#(0))
(test-assert \tuple(range(4))==`'#(0 1 2 3))

(test-assert \bytes()==`'#u8())
(test-assert \bytes(range(1))==`'#u8(0))
(test-assert \bytes(range(4))==`'#u8(0 1 2 3))

(test-assert \""==`"")
(test-assert \"a"==`"a")
(test-assert \"A B"==`"A B")

(test-assert \dict()==`(list->table '()))
(test-assert \dict([tuple(["a", 11])])==`(list->table '(("a" . 11))))
(test-assert \dict([tuple(["a", 11]),tuple(["b", 22])])==`(list->table '(("a" . 11) ("b" . 22))))

;;;============================================================================
;(shell-command (string-append "kill -9 " (number->string (##os-getpid))))
