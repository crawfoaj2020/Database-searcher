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
      (hosted-page "Confirm delete" 
        '()
        c1 c2 ...)]))

(define (database-confirm)
  (respond `(p "Are you sure you wish to delete this database?")
    `(p "The database will not be removed from memory, just from this application")
    (link "saved?type=database&sql=&limit=100&offset=0" "Cancel")))

(define (search-confirm)
  (respond `(p "Are you sure you want to remove this search?")
    (link "saved?type=search&sql=&limit=100&offset=0" "Cancel")))

(define (dispatch)
  (match (get-param "type")
    ["database" (database-confirm)]
    ["search" (search-confirm)]))

(dispatch)
