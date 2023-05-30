;;
;; Copyright 2023, Jaidyn Levesque <jadedctrl@posteo.at>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;

(import scheme
        (chicken io) (chicken string) (chicken irregex) (chicken pretty-print)
        srfi-1
        (prefix chatdir chatdir:)
        (prefix intarweb intarweb:)
        (prefix spiffy spiffy:)
        (prefix uri-common uri:))


(define (sanitize-html text)
  (irregex-replace/all
   "{" (spiffy:htmlize text) "&#123;"))


(define (html-from-template template-file variables-alist)
  (let ([text (call-with-input-file template-file
                (lambda (in-port) (read-string #f in-port)))])
    (map (lambda (variable-pair)
           (set! text
             (irregex-replace/all
              (string-append "{{" (car variable-pair) "}}")
              text
              (cdr variable-pair))))
         variables-alist)
    text))


(define (room-listing-html irc-dir)
  (html-from-template
   "templates/room-list.html"
   `(("LIST_ITEMS"
      . ,(reduce-right
          string-append
          ""
          (map (lambda (room)
                 (html-from-template
                  "templates/room-list-item.html"
                  `(("ROOM_TITLE" . ,room)
                    ("ROOM_ID" . ,room)
                    ("LAST_MESSAGE" . "nekonata: Lorem ipso facto…"))))
               (chatdir:channels irc-dir)))))))


(define (http-get-room irc-dir #!optional request continue)
  (spiffy:send-response status: 'ok
                        body: (room-listing-html irc-dir)))


;; Handle all GET requests.
(define (http-get irc-dir request continue)
  (let ([path (uri:uri-path (intarweb:request-uri request))])
    (cond [(starts-with? path '(/ "room"))
           (http-get-room irc-dir request continue)]
          [(starts-with? path '(/ "style.css"))
           (spiffy:send-response
            status: 'ok
            body: (call-with-input-file "templates/style.css"
                    (lambda (in-port) (read-string #f in-port)))
            headers: '((content-type "text/css")))]
          [(list= equal? path '(/))
           (spiffy:send-response status: 'ok body: "<h1>Index ♥</h1>")]
          [#t
           (continue)])))


;; Handle all POST requests.
(define (http-post irc-dir request continue)
  (continue))


;; Creates a handler for all HTTP requests, with the given IRC dir.
(define (make-http-handler irc-dir)
  (lambda (continue)
    (let* ([request (spiffy:current-request)]
           [request-type (intarweb:request-method request)])
      (cond [(eq? request-type 'GET)
             (http-get irc-dir request continue)]
            [(eq? request-type 'POST)
             (http-post irc-dir request continue)]
            [#t
             (intarweb:continue)]))))


;; Kick off the HTTP server.
(define (start-server irc-dir)
  (spiffy:vhost-map `((".*" . ,(make-http-handler irc-dir))))
  (spiffy:root-path irc-dir)
  (spiffy:start-server port: 8080))


;; Check if a `list` begins with the elements of another list.
(define (starts-with? list list-start #!optional (= equal?))
  (list= =
         (take list (length list-start))
         list-start))


 (start-server "/home/jaidyn/Chat/IRC/leagueh/")
