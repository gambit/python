(import (_six python)
        (github.com/gambit/python))

\import requests

(define (get-json url) \requests.get(`url).json())

(pp (table->list (get-json "https://jsonplaceholder.typicode.com/todos/1")))

;; output:
;;
;; (("userId" . 1) ("title" . "delectus aut autem") ("completed" . #f) ("id" . 1))
