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

(define (intial-setup)
  (respond `(form
    (table
     (tr (th (p "Field")) (th (p "Value")))
     (tr (td (p "Name")) (td (p (textarea (@ (id "name") (name "name") (class "textBox"))))))
     (tr (td (p "Description")) (td (p (textarea (@ (id "desc") (name "desc") (class "desc"))))))
     (tr (td (p "File")) (td (input (@ (name "path") (class "pathFeild") (type "file") (id "path"))))))
    (input (@ (id "filePath") (name "filePath") (value "Hi")))
    (p (button (@ (type "submit")) "Save"))
    (select (@ (name "cols") (class "cols"))
      (option "a")
      (option "b"))
    (script "$('.cols').bind('change', testMethod).trigger('change')"))))


(define (update-path name desc file)
  ;; (let ((path (string-param "path-val" params))) 
  ;; ;(log-path path)
  ;; (respond1
  ;;  `(p "Path updated. New path:")
  ;;  `(p ,path) `(script "location.reload(); document.getElementById('path').value = #f;"))))
  (let ([val (string-param "filePath" params)])
    (respond `(p ,val)
     ;`(script "document.getElementById('filePath').value = 'apples'")
      `(p (button (@ (type "submit")) "Go")))))

(define (dispatch)
  (let ([name (string-param "name" params)]
        [desc (string-param "desc" params)]
        [file (string-param "path" params)])
    (if name
        (update-path name desc file)
        (intial-setup))))

(dispatch)
