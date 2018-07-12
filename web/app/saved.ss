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

(http:include "displayQuery.ss")
(import (helpers))

(define (get-page-name)
  (match (get-param "type")
    ["database" "Saved databases"]
    ["search" "Saved searches"]
    [,_ (raise `#(invalid-type))]))

(define (respond:error reason)
  (respond
   (match reason
     [#(invalid-type) (section "Fail" `(p "Invalid type"))]
     [,_ (section "Critical error" `(p ,(exit-reason->english reason)))])))

(define (home-link last-sql)
  (match (get-param "type")
    ["database" (link "addDatabase" "Add database")]
    ["search" (void)]))

(define (dispatch)
  (let ([limit (integer-param "limit" 0 params)]
        [offset (integer-param "offset" 0 params)]
        [search-sql "Select name, description, sqlite, null as [delete] from searches order by name collate nocase"]
        [data-sql "Select name, description, file_path, null as [delete] from databases order by name collate nocase"]
        [type (get-param "type")]
        [sql (string-param "sql" params)]
        [search-func (lambda (name desc sqlite delete)
                       (list name desc
                         (link (format "query-db?limit=100&offset=0&sql=~a" sqlite) (format "~a" sqlite))
                         (link (format "confirm-delete?type=search&val=~a" sqlite)  "Delete")))]
        [data-func (lambda (name desc filePath delete)
                     (list name desc
                       (link (format "updatePath?val=~a" filePath) (format "~a" filePath))
                       (link (format "confirm-delete?type=database&val=~a" filePath) "Delete")))]) 
    
   (let ([sql (if (previous-sql-valid? sql)
                   sql
                   (match type
                     ["database" data-sql]
                     ["search" search-sql]))]
         [func (match type
                 ["database" data-func]
                 ["search" search-func])])
        
      
    (with-db [db (log-path) SQLITE_OPEN_READONLY]
          (do-query db sql limit offset type func)))))

(dispatch)
