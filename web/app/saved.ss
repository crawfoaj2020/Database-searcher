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
    ["folder" "Please select the folder type"]
    ["fileSelect" "Please select all desired file types"]
    ["fileEdit" "Edit file types"]
    [,_ (raise `#(invalid-type))]))

(define (respond:error reason)
  (respond
   (match reason
     [#(invalid-type) (section "Fail" `(p "Invalid type"))]
     [,_ (section "Critical error" `(p ,(exit-reason->english reason)))])))

(define (home-link last-sql)
  (match (get-param "type")
    ["database" (link "addDatabase" "Add database")]
    ["search" (void)]
    ["folder" (link "addFolder" "Add a new folder type")]
    ["fileSelect" (link "saved?type=fileEdit&sql=&limit=100&offset=0" "Edit file types")]
    ["fileEdit" (link "addFile" "Add a new file type")]))

(define (dispatch)
  (let ([limit (integer-param "limit" 0 params)]
        [offset (integer-param "offset" 0 params)]
        [search-sql "Select name, description, sqlite, null as [delete] from searches order by name collate nocase"]
        [data-sql "Select name, description, file_path, null as [delete] from databases order by name collate nocase"]
        [folder-sql "Select name, [List of file types], Header, null as [delete] from [folder_types] order by name collate nocase"]
        [file-select-sql "select null as [select], [File prefix], [column names], [example], [Regular expression] from [file_types] order by [File prefix] collate nocase"]
        [file-edit-sql "select [File prefix], [column names], [example], [Regular expression], null as [delete] from [file_types] order by [File prefix] collate nocase"]
        [type (get-param "type")]
        [sql (string-param "sql" params)]
        [search-func (lambda (name desc sqlite delete)
                       (list
                        (link (format "query-db?limit=100&offset=0&sql=~a" sqlite) (format "~a" name))
                        desc sqlite
                        (link (format "confirm-delete?type=search&val=~a" sqlite)  "Delete")))]
        [data-func (lambda (name desc filePath delete)
                     (list
                      (link (format "updatePath?val=~a" filePath) (format "~a" name))
                       desc filePath
                       (link (format "confirm-delete?type=database&val=~a" filePath) "Delete")))]
        [folder-func (lambda (name ls header delete)
                       (list
                        (link (format "convert?name=~a" name) (format "~a" name))
                        ls header
                        (link (format "confirm-delete?type=folder&val=~a" name) "Delete")))]
        [file-select-func (lambda (select prefix cols example regularExp)
                     (list "" ;Change to checkbox
                       prefix cols example regularExp))]
        [file-edit-func (lambda (prefix cols example regularExp delete)
                     (list
                      prefix cols example regularExp
                      (link (format "confirm-delete?type=file&val=~a" prefix) "Delete")))])
                       
    
   (let ([sql (if (previous-sql-valid? sql)
                   sql
                   (match type
                     ["database" data-sql]
                     ["search" search-sql]
                     ["folder" folder-sql]
                     ["fileSelect" file-select-sql]
                     ["fileEdit" file-edit-sql]))]
         [func (match type
                 ["database" data-func]
                 ["search" search-func]
                 ["folder" folder-func]
                 ["fileSelect" file-select-func]
                 ["fileEdit" file-edit-func])])
        
      
    (with-db [db (log-path) SQLITE_OPEN_READONLY]
          (do-query db sql limit offset type func)))))

(dispatch)
