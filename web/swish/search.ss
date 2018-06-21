;;; Copyright 2018 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permirespot persons to whom the Software is
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
(include "c:/Users/AJCRAWFORD/Documents/searchLotsDirs/web/swish/runningQuery.ss")

(define (get-page-name)
  (let ([table (string-param "table")])
    (if table
        (string-append "Searching table: " (stringify table))
        "Search")))

(define (respond:error reason)
  (respond
   (match reason
     
     [#(search-term-or-column-empty) (section "Search failed" `(p "If you specify a column, you must specify a search term. Similarly, if you specify a search term you must specify a column"))]

     [#(min-or-max-empty) (section "Search failed" `(p "You must enter both a min and a max if you want to limit by time"))]
     
     [#(no-timestamp) (section "Search failed: no timestamp" `(p "Please select a table with that has a column named timestamp in order to search by timestamp"))]
     
     [,_
      (section "Query failed" `(p ,(exit-reason->english reason)))])))



;: SQL helpers
(define (construct-sql  search-table search-column search-term range-min range-max desc db order-col)
  (define (check-request-blank-vals)
    (cond
     [(or (and (not (string=? search-column ""))
               (string=? search-term ""))
          (and (string=? search-column "")
               (not (string=? search-term ""))))
      (raise `#(search-term-or-column-empty))]

     [(or (and (not (string=? range-min ""))
              (string=? range-max ""))
          (and(string=? range-min "")
              (not (string=? range-max ""))))
      (raise `#(min-or-max-empty))]))
         
  
  (define (removeTimestamp columns)
    (if (string-ci=? (car columns) "timestamp")
      (cdr columns)
      (cons (car columns) (removeTimestamp (cdr columns)))))
  (define (build-order all-cols)
    (let ([order-col (if (string=? order-col "")
                         "ROWID"
                         order-col)]
          [desc-or-asc (if desc
                         "DESC"
                         "ASC")])
      (string-append "ORDER by " order-col " "  desc-or-asc)))
  (define (build-search-str)
    (let ((double-quoted (string-replace search-term "'" "''")))
      (if (string=? search-column "")
        #f
        (string-append search-column " like  " "('" double-quoted "')"))))

  (define (build-time-range)
    (let ((range-min (string-replace range-min "'" "''"))
          (rande-max (string-replace range-max "'" "''")))
    (if (string=? range-min "")
        #f
        (string-append "datetime(timestamp/1000,'unixepoch','localtime')"
          "between ('" range-min "') and ('" range-max "')"))))

  (check-request-blank-vals)
  (let* ([all-cols (get-columns search-table db)]
         [ls (cons (build-order all-cols) '())]
         [time-range (build-time-range)]
         [ls (if time-range
                 (cons time-range ls)
                 ls)]

         [search-str (build-search-str)]
         [ls (cond [(and search-str time-range)
                    (let ((temp (cons "and" ls)))
                      (cons search-str temp))]
               [search-str (cons search-str ls)]
               [else ls])]
         
         [ls (if (or time-range search-str)
                 (cons "where" ls)
                 ls)]
         
         [ls (cons search-table ls)]
         [ls (cons "from" ls)]
         [timestamp?  (if (containsStr? all-cols "timestamp")
                          #t
                          #f)]
         [columns (if timestamp?
                      (removeTimestamp all-cols)
                      all-cols)]
         [ls (cons (slist->string columns ", ") ls)]
         [ls (if timestamp?
                 (cons "select datetime(timestamp/1000,'unixepoch','localtime') as timestamp," ls)
                 (cons "select" ls))])
    
    (if (and time-range (not (containsStr? all-cols "timestamp")))
        (raise `#(no-timestamp)))
    (slist->string ls " ")))

(define (remove-tags val)
    (match val
      [#(,table-name)
       (string->symbol table-name)]))

(define (get-columns table db)
  (define (table-info master-row)
    (match master-row
      [,table-name
        (map stringify (map column-info
                         (execute-sql db (format "pragma table_info(~s)" table-name))))]
      [,_ (raise `#(Invalid-table))]))
  (define (column-info table-info)
    (match table-info
      [#(,_ ,name ,type ,_ ,_ ,_)
       (string->symbol name)]))
  (table-info table))


;;Intial setup
(define (intial-setup db)
  (define (table-info master-row)
    (match master-row
      [#(,table-name)
       (cons
        (string->symbol table-name)
        (map column-info
          (execute-sql db (format "pragma table_info(~s)" table-name))))]))
  (define (column-info table-info)
    (match table-info
      [#(,_ ,name ,type ,_ ,_ ,_)
       (cons (string->symbol name) type)]))
  
  (define (make-table-drop-down)
    (let ((tables (map remove-tags (execute-sql db
                                      "select tbl_name from SQLITE_MASTER where type in (?, ?) order by tbl_name" "table" "view"))))
      `(select (@ (name "table") (id "table"))
         ,@(map (lambda (c) `(option ,(stringify c))) tables))))

  (define (make-col-drop-downs db-tables cont-name drop-name)
    (define (db-table->selection table)
      (match table
        [(,name . ,columns)
         `(div (@ (class ,cont-name))
         (div (@ (class ,(stringify name)))
           (select (@ (name ,drop-name) (class ,drop-name))
             (option "")
               ,@(map column->option columns))))]))
    (define (column->option column-type)
      (match column-type
        [(,column . ,type)
         `(option ,(stringify column))]))
    `(div
       ,@(map db-table->selection db-tables)))
    
  
  (let ([db-tables
         (map table-info
           (execute-sql db
             "select tbl_name from SQLITE_MASTER where type in (?, ?) order by tbl_name" "table" "view"))])
    (respond
     
     (section "Please enter the following fields"
     `(form (@ (method "get") (class "schema")) (table
             (tr (@ (style "text-align:center;"))
               (th (p "Field")) (th (p "Value")) (th (p "Notes")))
             (tr (td (p "Table")) (td (form ,(make-table-drop-down))) (td (p "Required")))
              (tr (td (p "Column")) (td ,(make-col-drop-downs db-tables "container" "cols")) (td (p "Select a table first")))
              (tr (td (p "Search term")) (td (p (textarea (@ (id "keyWord") (name "keyWord") (class "textBox"))
                                             ,""))) (td (p "% is used for any number of don't care characters")
                                                      (p "#% returns everything that starts with #")))
              (tr (td (p "Minimum date-time")) (td (p (textarea (@ (id "min") (name "min") (class "textBox"))
                                                        ,""))) (td (p "Inclusive") (p "Format yyyy-mm-dd hh:mm:ss")
                                                                 (p "For example, 2018-06-06 13:06:07")))
              (tr (td (p "Maximum date-time")) (td (p (textarea (@ (id "max") (name "max") (class "textBox"))
                                                        ,""))) (td (p "Inclusive")))
              (tr (td (p "Order by")) (td ,(make-col-drop-downs db-tables "order-contain" "orders")) (td (p "Leave blank for timestamp")))
              (tr (td (p "Desc")) (td (label (@ (class "checkbox-inline"))
                 (input (@ (name "desc")
                           (type "checkbox") (checked))))) (td (p "If sorting by timestamp shows most recent first"))))
        (input (@ (name "limit") (class "hidden") (value 100)))
        (input (@ (name "offset") (class "hidden") (value 0)))
        (input (@ (name "type") (class "hidden") (value "")))
        (input (@ (id "column") (name "column") (class "hidden") (value "")))
        (input (@ (id "order") (name "order") (class "hidden") (value "")))
        (p (button (@ (type "submit")) "Run Search"))
        (p (textarea (@ (id "sql") (name "sql") (class "hidden"))))
        (script "$('div.container').children().hide();
var select = document.getElementById('table');
select.addEventListener('change', updateColumnSearch, false);")
         (script "$('div.order-contain').children().hide();
var select = document.getElementById('table');
select.addEventListener('change', updateColumnOrder, false);")
        (script "$('.cols').bind('change', updateOtherFeildSearch).trigger('change')")
        (script "$('.orders').bind('change', updateOtherFeildOrder).trigger('change')"))
        )
     
     (section "Schema"
      (schema->html db-tables)))))

(define (home-link last-sql)
    `(a (@ (href "search"))
       "Return to search page"))


;;Runs each time something changes, calls intial-setup or do-query
(define (dispatch)
  (let ([keyword (string-param "keyWord")]
        [table (string-param "table")]
        [min (string-param "min")]
        [max (string-param "max")]
        [desc (find-param "desc")]
        [limit (integer-param "limit" 0)]
        [offset (integer-param "offset" 0)]
        [sql (string-param "sql")]
        [order-col (string-param "order")])
    (with-db [db (log-path) SQLITE_OPEN_READONLY]
      (cond
       [(previous-sql-valid? sql) (do-query db sql limit offset ""  (lambda x x))]
        [table
         (let ([column (string-param "column")])
           (match (catch (construct-sql table column keyword min max desc db order-col))
             [#(EXIT ,reason) (respond:error reason)]
             [,value (do-query db value limit offset "" (lambda x x))]))]
        [else (intial-setup db)]))))
    
    

(dispatch)

