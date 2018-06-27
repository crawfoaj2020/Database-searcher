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

;; HTML responses
(define-syntax respond
  (syntax-rules ()
    [(_ c1 c2 ...)
      (hosted-page "Add a new database"
        (list (css-include "css/saveSearch.css")
          (js-include "js/addData.js"))
        c1 c2 ...)]))

(define (respond:error reason)
  (respond
   (match reason
     [#(not-database) (section "insert failed" `(p "Invalid file type") `(p "valid file types are: .db, .db3, .sqlite"))]
     [,_ (section "insert failed" `(p ,(exit-reason->english reason)))])))

(define (intial-setup)
  (respond `(form
    (table
     (tr (th (p "Field")) (th (p "Value")))
     (tr (td (p "Name")) (td (p (textarea (@ (id "name") (name "name") (class "textBox"))))))
     (tr (td (p "Description")) (td (p (textarea (@ (id "desc") (name "desc") (class "desc"))))))
     (tr (td (p "File")) (td (p (textarea (@ (id "path") (name "path") (class "desc")))))))
     ;(tr (td (p "File")) (td (input (@ (name "path") (class "pathFeild") (type "file") (id "path"))))))
    ;(input (@ (id "filePath") (name "filePath") (value "Hi")))
    (p (button (@ (type "submit")) "Save")))))


(define (update-path name desc file)
  (unless (or (ends-with-ci? file ".db3")
              (ends-with-ci? file ".db")
              (ends-with-ci? file ".sqlite"))
    (raise `#(not-database)))
  (match (db:transaction 'log-db (lambda () (execute (format "insert into databases (name, description, file_path)
values ('~a', '~a', '~a')" name desc file))))
    [#(ok ,_) (begin (user-log-path file) (respond `(p "Save successful")))]
    [,error (respond:error error)]))


(define (dispatch)
  (let ([name (string-param-sql "name" params)]
        [desc (string-param-sql "desc" params)]
        [file (string-param-sql "path" params)])
    (if name
        (match (catch (update-path name desc file))
          [#(EXIT ,reason) (respond:error reason)]
          [,value value])
        (intial-setup))))

(dispatch)
