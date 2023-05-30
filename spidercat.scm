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


;; Santize text for placement in HTML.
(define (sanitize-html text)
  (irregex-replace/all
   "{" (spiffy:htmlize text) "&#123;"))


;; Generate HTML from a template-file, substituting in variables as appropriate.
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


;; Generate HTML for a listing of all rooms the user's joined.
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


;; Send response for a listing of joined rooms.
(define (http-get-room irc-dir #!optional request continue)
  (spiffy:send-response status: 'ok
                        body: (room-listing-html irc-dir)))


;; Send response for the / index.
(define (http-get-root #!optional irc-dir request continue)
  (spiffy:send-response status: 'ok body: "<h1>Index!!</h1>"))


;; Send a 404 response, with disappointed text.
(define (http-404 #!optional irc-dir request continue)
  (spiffy:send-response code: 404 body: "<h1>Sad!</h1>"))


;; Send the static style CSS.
(define (http-get-style #!optional irc-dir request continue)
  (spiffy:send-response
   status: 'ok
   body: (call-with-input-file "templates/style.css"
           (lambda (in-port) (read-string #f in-port)))
   headers: '((content-type "text/css"))))


;; An associative list of all GET handlers, to be used by assoc-by-path.
(define http-get-handlers
  `(((/ "room") . ,http-get-room)
    ((/ "style.css") . ,http-get-style)
    ((/ "*") . ,http-404)
    ((/ "") . ,http-get-root)))


;; Get a pair from an associative list based on the closest match to the
;; given path. Wild-cards acceptable! For example…
;;     '(/ "dad" "mom") matches, in order of precedence:
;;     '(/ "dad" "mom") '(/ "dad" "*") '(/ "*")
(define (assoc-by-path path-list alist #!optional (top-level #t))
  (print path-list)
  (let* ([our-list=
          (lambda (a b)
            (list= equal? a b))]
         [parent-path (drop-right path-list 1)]
         [exact
          (and top-level
               (assoc path-list
                      alist our-list=))]
         [inexact
          (assoc (append parent-path '("*"))
                 alist our-list=)])
    (or exact
        inexact
        (and (not (null? parent-path))
             (assoc-by-path parent-path alist #f)))))


;; Handle all GET requests.
(define (http-get irc-dir request continue)
  (let* ([path (uri:uri-path (intarweb:request-uri request))]
         [handler (assoc-by-path path http-get-handlers)])
    (if handler
        (apply (cdr handler) (list irc-dir request continue))
        (continue))))


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


;; (start-server "/home/jaidyn/Chat/IRC/leagueh/")
