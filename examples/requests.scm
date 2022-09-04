(import (_six python)
        (github.com/gambit/python))

\import requests

(define-type Response
  read-only:
  unprintable:
  res cookies encoding headers json status-code text url)

(define (Response-conv obj)
  (make-Response obj
                \(`obj).cookies
                \(`obj).encoding
                \(`obj).headers
                (lambda () \(`obj).json())
                \(`obj).status_code
                \(`obj).text
                \(`obj).url))

(PyObject*-register-converter "Response" Response-conv)

(define (get url)
  \requests.get(`url))

(define r (get "https://jsonplaceholder.typicode.com/todos/1"))
(define json ((Response-json r)))

(define (get-json url)
  \requests.get(`url).json())

\print(`(Response-res r).json())
;; {'userId': 1, 'id': 1, 'title': 'delectus aut autem', 'completed': False}
