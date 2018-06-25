;;; Copyright 2018 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(import (helpers))
(http:include "components.ss")

(define (uptime)
  (let* ([x (real-time)]
         [milliseconds (remainder x 1000)]
         [x (quotient x 1000)]
         [seconds (remainder x 60)]
         [x (quotient x 60)]
         [minutes (remainder x 60)]
         [x (quotient x 60)]
         [hours (remainder x 24)]
         [x (quotient x 24)]
         [days x])
    (format "~a day~:p, ~a hour~:p, ~a minute~:p, ~a second~:p"
      days hours minutes seconds)))

;; HTML responses
(define-syntax respond1
  (syntax-rules ()
    [(_ c1 c2 ...)
      (hosted-page (format "~a ~a" software-product-name software-version)
        (list (css-include "css/index.css")
          (css-include "css/query-db.css")
          (js-include "js/index.js"))
        c1 c2 ...)]))

(define (do-home)
  (respond1
   (section "Summary"
   `(p "Snapshot from " ,(date-and-time))
   `(p "Uptime: " ,(uptime))
   `(p "These pages are designed for a 1680 by 1050 resolution using Firefox, Opera, Chrome, or Safari."))
    (section "Setup connection to a new database"
      `(form (@ (method "get") (class "data-path"))
         (p "Select the sqlite database to access")
         (input (@ (name "path") (class "pathFeild") (type "file") (id "path")))
         (radiogroup
         (input (@ (name "basic-import") (type "radio") (checked))) ;"Basic import. Will not log current events")
         (input (@ (name "log-import") (type "radio")))) ;"Log current events. May alter database schema without losing information")
         (input (@ (name "path-val") (id "pathVal") (value "Mine")))
         (p (button (@ (type "submit")) "Submit"))))))
         ;(script "$('.pathFeild').bind('change', setPath).trigger('change')")
         ;(script "document.getElementById('pathVal').value = 'Work';"))))) 

(define (update-path)
  (let ((path (string-param "path-val" params))) 
  ;(log-path path)
  (respond1
   `(p "Path updated. New path:")
   `(p ,path) `(script "location.reload(); document.getElementById('path').value = #f;"))))

(define (dispatch)
  (let ([path (string-param "path" params)])
    (if path
        (update-path)
        (do-home))))


(dispatch)
