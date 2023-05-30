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
        srfi-1
        (prefix chatdir chatdir:)
        (prefix intarweb intarweb:)
        (prefix spiffy spiffy:)
        (prefix uri-common uri:))


;; Handle all GET requests.
(define (get-handler irc-dir request continue)
  (let ([path (uri:uri-path (intarweb:request-uri request))])
    (if (starts-with? path '(/))
        (spiffy:send-response status: 'ok body: "<h1>Index ♥</h1>")
        (continue))))


;; Handle all POST requests.
(define (post-handler irc-dir request continue)
  (continue))


;; Creates a handler for all HTTP requests, with the given IRC dir.
(define (make-http-handler irc-dir)
  (lambda (continue)
    (let* ([request (spiffy:current-request)]
           [request-type (intarweb:request-method request)])
      (cond [(eq? request-type 'GET)
             (get-handler irc-dir request continue)]
            [(eq? request-type 'POST)
             (post-handler irc-dir request continue)]
            [#t
             (intarweb:continue)]))))


;; Kick off the HTTP server.
(define (start-server irc-dir)
  (spiffy:vhost-map `((".*" . ,(make-http-handler irc-dir))))
  (spiffy:start-server port: 8080))


;; Check if a `list` begins with the elements of another list.
(define (starts-with? list list-start #!optional (= eq?))
  (list= =
         (take list (length list-start))
         list-start))


 (start-server "/home/jaidyn/Chat/IRC/leagueh/")
