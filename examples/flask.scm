(import (_six python))
(import (github.com/udem-dlteam python))

\from flask import Flask

\app=Flask(__name__)

(define (home) "hello, world!")

\app.route("/")(`home)

(define flask-thread
  (thread
   (lambda ()
     (\app.run host: "127.0.0.1"
               port: 5000
               threaded: #f))))
