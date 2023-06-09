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
        (chicken file) (chicken io) (chicken sort) (chicken string)
        (chicken irregex) (chicken pretty-print)
        srfi-1 srfi-19
        (prefix chatdir chatdir:)
        (prefix intarweb intarweb:)
        (prefix spiffy spiffy:)
        (prefix uri-common uri:))


;; Santize text for placement in HTML.
(define (html-encode-string text)
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
                 (room-list-item-html irc-dir room))
               (chatdir:channels irc-dir)))))))


(define (room-list-item-html irc-dir room)
  (let* ([messages (channel-messages-sorted irc-dir room)]
         [last-message (if (null? messages)
                           #f (last messages))]
         [message-text (if last-message
                           (car last-message) "")]
         [message-sender (if last-message
                             (or (alist-ref 'user.chat.sender
                                            (cdr last-message))
                                 "")
                             "")]
         [message-time
          (if last-message
              (date->string (alist-ref 'user.chat.date
                                       (cdr last-message))
                            "[~H:~M:~S]")
              "")])
    (html-from-template
     "templates/room-list-item.html"
     `(("ROOM_TITLE" . ,(html-encode-string room))
       ("ROOM_ID" . ,(uri:uri-encode-string room))
       ("LAST_MSG" . ,message-text)
       ("LAST_TIME" . ,message-time)
       ("LAST_MSG_SENDER" . ,message-sender)))))



;; “Send” a message to the given chatdir root, simply by creating a file.
;; That was easy!
(define (send-message irc-dir channel message)
  (with-output-to-file
      (string-append irc-dir "/" channel "/.in/a")
    (lambda ()
      (write-string message))))


;; Returns all of a channel's messages — in alist format, with parsed datetimes.
(define (channel-messages irc-dir channel)
  (map (lambda (msg-alist)
         (let ([date-str (alist-ref 'user.chat.date (cdr msg-alist))])
           (append
            (list (car msg-alist))
            (alist-update 'user.chat.date
                          (string->date date-str "~Y-~m-~dT~H:~M:~S~z")
                          (cdr msg-alist)))))
       (map (lambda (message)
              (chatdir:channel-message-get irc-dir channel message))
            (chatdir:channel-messages irc-dir channel))))


;; Returns all of a channel's messages, sorted in order of datetime.
(define (channel-messages-sorted irc-dir channel)
  (sort
   (channel-messages irc-dir channel)
   (lambda (a b)
     (let ([date-a (alist-ref 'user.chat.date (cdr a))]
           [nano-a (alist-ref 'user.chat.date.nanoseconds (cdr a))]
           [date-b (alist-ref 'user.chat.date (cdr b))]
           [nano-b (alist-ref 'user.chat.date.nanoseconds (cdr b))])
       (cond [(and (date=? date-a date-b)
                   nano-a nano-b)
              (> (string->number nano-b)
                  (string->number nano-a))]
             [#t
              (date<? date-b date-a)])))))


(define (channel-online-users irc-dir channel)
  (directory
   (string-append irc-dir "/" channel "/.users/online/")))


(define (room-users-html irc-dir channel)
  (html-from-template
   "templates/room-user-list.html"
   `(("ROOM_TITLE" . ,(uri:uri-decode-string channel))
     ("LIST_ITEMS"
      . ,(reduce-right
          string-append ""
          (map (lambda (user)
                 (room-users-item-html irc-dir channel user))
               (channel-online-users
                irc-dir
                (uri:uri-decode-string channel))))))))


(define (room-users-item-html irc-dir channel user)
  (html-from-template
   "templates/room-user-list-item.html"
   `(("USER_NAME" . ,user))))


(define (room-index-html irc-dir channel)
  (html-from-template
   "templates/room-index.html"
   `(("ROOM_TITLE" . ,(uri:uri-decode-string channel))
     ("ROOM_ID" . ,(uri:uri-encode-string channel)))))


(define (room-send-html)
  (html-from-template "templates/room-send.html" '()))


;; Generate the HTML listing a room's chat messages.
(define (room-messages-html irc-dir channel)
  (html-from-template
   "templates/room-messages.html"
   `(("ROOM_TITLE" . ,(uri:uri-decode-string channel))
     ("LIST_ITEMS"
      . ,(reduce-right
          string-append ""
          (map (lambda (message)
                 (room-messages-item-html irc-dir channel message))
               (channel-messages-sorted
                irc-dir
                (uri:uri-decode-string channel))))))))


;; Generate the HTML for a specific message in a specific room.
;; Used to substitute {{LIST_ITEMS}} in the room-messages template.
(define (room-messages-item-html irc-dir channel message)
  (if (and (list? message)
           (string? (car message)))
      (html-from-template
       "templates/room-messages-item.html"
       `(("MESSAGE_SENDER"
          . ,(html-encode-string
              (or (alist-ref 'user.chat.sender (cdr message)) "")))
         ("MESSAGE_DATE"
          . ,(html-encode-string
              (date->string
               (alist-ref 'user.chat.date (cdr message))
               "~Y-~m-~d")))
         ("MESSAGE_TIME"
          . ,(html-encode-string
              (date->string
               (alist-ref 'user.chat.date (cdr message))
               "~H:~M:~S")))
         ("MESSAGE_TEXT"
          . ,(html-encode-string
              (car message)))))
      ""))


;; Send response for a listing of joined rooms.
(define (http-get-rooms-list irc-dir #!optional request path)
  (spiffy:send-response status: 'ok
                        body: (room-listing-html irc-dir)))



(define (http-get-room-dir irc-dir #!optional request path)
  (let* ([channel (third path)]
         [channel? (member channel (chatdir:channels irc-dir))]
         [sub-path (if (eq? (length path) 4)
                       (fourth path) #f)])
    (cond
     [(not channel?)
      (spiffy:send-response code: 404
                            body: "<h1>That's not a channel, smh!!</h1>")]
     [(equal? sub-path "users")
      (spiffy:send-response status: 'ok
                            body: (room-users-html irc-dir channel))]
     [(equal? sub-path "messages")
      (spiffy:send-response status: 'ok
                            body: (room-messages-html irc-dir channel))]
     [(equal? sub-path "send")
      (spiffy:send-response status: 'ok
                            body: (room-send-html))]
     [(or (not sub-path) (string=? sub-path ""))
      (spiffy:send-response status: 'ok
                            body: (room-index-html irc-dir channel))])))


(define (http-post-room-dir irc-dir #!optional request path)
  (let* ([channel (third path)]
         [request-data (intarweb:read-urlencoded-request-data request 50000)])
    (if (alist-ref 'message request-data)
        (begin
         (send-message irc-dir channel (alist-ref 'message request-data))
         (sleep 1)))
    (http-get-room-dir irc-dir request (list '/ "room" channel "messages"))))


;; Send response for the / index.
(define (http-get-root #!optional irc-dir request path)
  (spiffy:send-response status: 'ok body:
                        (html-from-template "templates/index.html" '())))


;; Send a 404 response, with disappointed text.
(define (http-404 #!optional irc-dir request path)
  (spiffy:send-response code: 404 body: "<h1>Sad!</h1>"))


;; Send the static style CSS.
(define (http-get-style #!optional irc-dir request path)
  (spiffy:send-response
   status: 'ok
   body: (call-with-input-file "templates/style.css"
           (lambda (in-port) (read-string #f in-port)))
   headers: '((content-type "text/css"))))


;; An associative list of all GET handlers, to be used by assoc-by-path.
(define http-get-handlers
  `(((/ "room") . ,http-get-rooms-list)
    ((/ "room" "*") . ,http-get-room-dir)
    ((/ "style.css") . ,http-get-style)
    ((/ "*") . ,http-404)
    (("*") . ,http-get-root)))


;; An associative list of POST handlers, to be used by assoc-by-path.
(define http-post-handlers
  `(((/ "room" "*") . ,http-post-room-dir)))


;; Get a pair from an associative list based on the closest match to the
;; given path. Wild-cards acceptable! For example…
;;     '(/ "dad" "mom") matches, in order of precedence:
;;     '(/ "dad" "mom") '(/ "dad" "*") '(/ "*")
(define (assoc-by-path path-list alist #!optional (top-level #t))
  (let* ([our-list=
          (lambda (a b)
            (list= equal? a b))]
         [path-list
          (if (eq? (string-length (last path-list)) 0)
              (drop-right path-list 1)
              path-list)]
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
        (apply (cdr handler) (list irc-dir request path))
        (continue))))


;; Handle all POST requests.
(define (http-post irc-dir request continue)
    (let* ([path (uri:uri-path (intarweb:request-uri request))]
           [handler (assoc-by-path path http-post-handlers)])
     (if handler
         (apply (cdr handler) (list irc-dir request path))
         (continue))))


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
