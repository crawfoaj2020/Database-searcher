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

(http:include "components.ss")
(import (helpers))

;; HTML responses
(define-syntax respond
  (syntax-rules ()
    [(_ c1 c2 ...)
      (hosted-page "Save search" 
        (list (css-include "css/saveSearch.css"))
        c1 c2 ...)]))

(define (intial-setup)
  (let ([sql (get-param "sql")])
  (respond `(form
    (table
     (tr (th (p "Field")) (th (p "Value")))
     (tr (td (p "Name")) (td (p (textarea (@ (id "name") (name "name") (class "textBox"))))))
     (tr (td (p "Description")) (td (p (textarea (@ (id "desc") (name "desc") (class "desc")))))))
    (p (button (@ (type "submit")) "Save"))
    (input (@ (id "sql") (name "sql") (class "hidden") (value ,sql)))))))

(define (save-query name desc sql)
  ;; (with-db [db (log-path) SQLITE_OPEN_READWRITE] 
  ;; (let* ([stmt (sqlite:prepare db "insert into searches (name, description, sqlite) values (?, ?, ?)")]
  ;;        [boundStm (sqlite:bind stmt (list name desc sql))])
  ;;   (sqlite:execute "insert into searches (name, description, sqlite) values ('test', 'ab', 'lastVal')" '())
  (define (execute-insert)
    (sqlite:step "insert into searches (name, description, sqlite) values ('OBVIOUS', 'ab', 'lastVal')"))
  ;; (match (catch (execute-insert))
  ;;   [#(EXIT ,reason) (respond `(p ,reason))]
  ;;   [,_
    
    (respond `(p "Save not yet implemented")))
       

(define (dispatch)
  (let ([name (string-param "name" params)]
        [desc (string-param "desc" params)]
        [sql (get-param "sql")])
    (if name
        (save-query name desc sql)
        (intial-setup))))

(dispatch)
  
