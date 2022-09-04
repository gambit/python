(import (_six python)
        (github.com/gambit/python))

\from flask import Flask

\app=Flask(__name__)

(define (home)
  (string-append "This is Gambit "
                 (system-version-string)))

\app.route("/")(`home)

(define flask-thread
  (thread
   (lambda ()
     (\app.run host: "127.0.0.1"
               port: 5000
               threaded: #f))))

(thread-sleep! 1) ;; wait for Flask server to start

;; Connect to the Flask server

(let ((conn (open-tcp-client "127.0.0.1:5000")))
  (print port: conn "GET / HTTP/1.1\r\n\r\n")
  (force-output conn)
  (pp (read-line conn #f))
  (close-port conn))

;; output:
;;
;;  * Serving Flask app '__main__'
;;  * Debug mode: off
;; WARNING: This is a development server. Do not use it in a production deployment. Use a production WSGI server instead.
;;  * Running on http://127.0.0.1:5000
;; Press CTRL+C to quit
;; 127.0.0.1 - - [04/Sep/2022 16:04:57] "GET / HTTP/1.1" 200 -
;; "HTTP/1.1 200 OK\r\nServer: Werkzeug/2.2.2 Python/3.9.13\r\nDate: Sun, 04 Sep 2022 20:04:57 GMT\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: 34\r\nConnection: close\r\n\r\nThis is Gambit v4.9.4-64-g9c22ed86"
