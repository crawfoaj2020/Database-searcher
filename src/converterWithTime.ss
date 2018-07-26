
;;Regular expressions
(define header-re (pregexp "Method = (.*)\\nLogged in user = (.*)\\nStarted (.*)\\nUnit serial number = (.*)\\nPod1 head serial number = (.*)\\nPod Last validated (.*)\\nPod2 head serial number = (.*)\\nPod Last validated (.*)"))
(define start-row-pattern (pregexp "(\\n)(\\d\\d/\\d\\d/\\d\\d\\d\\d \\d\\d:)"))
(define newline-pattern (pregexp "(.*)\\n(.*)"))
(define pattern (pregexp "yuiop([0-9 :/]*),(.*?)(?:qwert|$)"))
(define file-name (pregexp "([A-z]*8?[A-z]*)[0-9]+.*\\.log"))


(define (stringify x) (format "~a" x))


(define string-replace 
   (lambda (s match replacement)
      (let ((ll (string->list s)))

             (let ((z (map (lambda (x)
                              (if (string-ci=? (stringify x) match)
                                  (string->list replacement)
                                  x))
                           ll)))
                (list->string (flatten z))))))


(define (flatten list)
   (cond ((null? list) '())
         ((list? (car list)) (append (flatten (car list)) (flatten (cdr list))))
         (else
          (cons (car list) (flatten (cdr list))))))




(define (seperate-rows str)
  (let ([start-time (real-time)]
        [start-row-replace "qwertyuiop\\2"]
        ;Currently removes internal newlines. If need, change to \\1 unique pattern \\2,
        ;then later string-replace back to newline
        [newline-replace "\\1\\2"])
    (let* ([rows-marked (pregexp-replace* start-row-pattern str start-row-replace)]
           [newlinesRemoved (pregexp-replace* newline-pattern rows-marked newline-replace)]
           [double-quotes (string-replace newlinesRemoved "'" "''")]
           [search (pregexp-match-all pattern double-quotes)])
      (display (- (real-time) start-time))
      (display " seperate-rows\n")
      search)))

(define (pregexp-match-all pattern str)
  (define (helper sub-start sub-end found-matches)
    (let ([match-loc (pregexp-match-positions pattern str sub-start sub-end)])
      (if match-loc
          (match match-loc
            [((,start . ,end)(,g1start . ,g1end)(,g2start . ,g2end))
             (let* ([col1 (substring str g1start g1end)]
                    [col2 (substring str g2start g2end)]
                    [col-list (list col1 col2)])
               (helper end sub-end (cons col-list found-matches)))]
            [((,start . ,end))
             (helper end sub-end (cons (substring str start end) found-matches))]
            [,_ match-loc])
          found-matches)))
  
  (helper 0 (string-length str) '()))

(define (process-header contents)
  
  (let ([search (pregexp-match header-re contents)])
    (match search
      [(,fullResult . ,rest) rest])))

(define (make-formated-string ls)
  (match ls
    [() ""]
    [(,first) (string-append "'" first "'")]
    [(,first . ,rest) (string-append "'" first "', " (make-formated-string rest))]))

(define (read-file-to-string file-path)
  (let ([start-time (real-time)]
        [tx (make-transcoder (utf-8-codec) (eol-style lf)
                  (error-handling-mode replace))]
        [bytevector (read-file file-path)])
    (display (- (real-time) start-time))
    (display " read-file\n")
    (bytevector->string bytevector tx)))

(define (get-method contents)
  (match contents
    [(,method . ,rest) method]))

(define (insert-header contents db)
  (let ([start-time (real-time)]
        [comma-seperated (make-formated-string contents)]
        [cols "Method, [Logged in User], Started, [Unit serial number], [Pod1 head serial number], [Pod1 Last validated], [Pod2 head serial number], [Pod2 last validated]"])
    (execute-sql db (format "insert into Runs (~a) values (~a)" cols comma-seperated))
    (let ([rowid (execute-sql db "select last_insert_rowid()")])
      (display (- (real-time) start-time))
    (display " Insert-header\n")
      (match rowid
        [(#(,num)) num]))))

(define (processfile table-name file-path db prepared-insert)
  (let* ([contents (read-file-to-string file-path)]
         [processed (seperate-rows contents)]
         [processed-header (process-header contents)]
         [method (get-method processed-header)]
         [run-number (insert-header processed-header db)])
    (insert-file-contents table-name processed method run-number db prepared-insert)))

(define (fullConvert src-path db)
  (define (process-each-file remaining-files existing-tables)
    (match remaining-files
      [((,name . ,num) . ,rest)
       (let* ([short-name (get-name name)]
              [path (path-combine src-path name)]
              [prepared (get-second-val short-name existing-tables)])
         (cond [(not short-name) (process-each-file rest existing-tables)] ;Wrong file format, skip
           [prepared ;Table and prepared statement already created
            (begin (processfile short-name path db prepared)
                   (process-each-file rest existing-tables))]

           [else ;Need to create table and prepare statment
            (let*  ([table (create-table short-name)]
                    [prepared-insert (sqlite:prepare db (format "insert into ~a ([Run number], Method, dateTime, desc) values (?, ?, ?,?)" short-name))]
                    [new-table (cons short-name prepared-insert)])
              (processfile short-name path db prepared-insert)
              (process-each-file rest (cons new-table existing-tables)))]))]
      
      [() "Finsihed?"]))

  (define (get-name full-name)
    (let ([pattern-match (pregexp-match file-name full-name)])
      (match pattern-match
        [(,full ,name) name]
        (#f #f))))

  (define (get-second-val name list)
    (match list
     [((,key . ,value) . ,rest)
            (if (string=? name key)
                value
                (get-second-val name rest))]
      [() #f]
      [,_ list]))

  (define (create-table table-name)
    (let ([sql (format "create table if not exists ~a ([Run number] integer, Method text, dateTime text, desc text, foreign key([Run number]) references Runs([Unique Run Number]))" table-name)])
      (display (format "Created table: ~a\n" table-name))
      (execute-sql db sql)))
  
  (let ([file-list (list-directory src-path)])
      (process-each-file file-list '())))

(define (insert-file-contents table-name contents method run-number db prepared-insert)  
  (define (insert-row row-contents)
    (match row-contents
      [(,date ,desc)
       (sqlite:execute prepared-insert (list run-number method date desc))]))
           
  (define (insert-remaining remaining)
    (match remaining
      [(,last) (insert-row last)]
      [(,cur-row . ,rest) (begin (insert-row cur-row) (insert-remaining rest))]))

  (let ([start-time (real-time)])
    (insert-remaining contents)
    (display (- (real-time) start-time))
    (display " insert file contents\n")))


(define (set-up-conversion folder db)
  (execute-sql db "create table if not exists  Runs ([Unique Run Number] integer primary key, Method text, [Logged in User] text, Started text, [Unit serial number] text, [Pod1 head serial number] text, [Pod1 Last validated] text, [Pod2 head serial number] text, [Pod2 last validated] text)")
  (execute-sql db "PRAGMA synchronous = OFF")
  (fullConvert folder db))


(define (make-db-and-convert folder db-path)
  (db:start&link 'new-db db-path 'create)
  (db:stop 'new-db)
  (with-db [db db-path SQLITE_OPEN_READWRITE]
    (set-up-conversion folder db)))


(define (test-convert)
  (let (;[folder "C:\\Users\\AJCRAWFORD\\Documents\\Example from Praveena\\R0273091_2018.06.21_1236\\Logs\\Biomek Logs\\Logs"]
        [start-time (real-time)]
        [folder "C:\\Users\\AJCRAWFORD\\Documents\\Python\\testFiles\\Lots small"]
        [dest "C:\\Users\\AJCRAWFORD\\Documents\\Python\\testFiles\\ResultsSmallSynchronousOff.db3"])
    (make-db-and-convert folder dest)
    (display (- (real-time) start-time))))

(test-convert)


