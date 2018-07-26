(define (getUniqueValue info-db col table pkCol pkVal)
  (define (get-results next-row)
    (let lp ([results '()])
      (match (next-row)
        [#f (reverse results)]
        [,row (lp (cons row results))])))

  (match-let*
   ([,stmt (sqlite:prepare info-db (format "select ~a from ~a where ~a = ?" col table pkCol))]
    [,_ (sqlite:bind stmt (list pkVal))]
    [,results (get-results (lambda () (sqlite:step stmt)))]
    [,results-ls (vector->list (car results))])
   (car results-ls)))

(define (make-tables prefix-ls info-db cDest)
  (define (make-table table-name)
    (let* ([cols (string-append "Run number, Method, " (getUniqueValue info-db "[column names]" "file_types" "[File prefix]" table-name))]
           [cols (split cols ", ")]
           [regular-exp (getUniqueValue info-db "[Regular expression]" "file_types" "[File prefix]" table-name)])
      regular-exp)
    )
  (map make-table prefix-ls))
