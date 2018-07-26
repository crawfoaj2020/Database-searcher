;;Regular expressions
(define header-re (pregexp "Method = (.*)\\nLogged in user = (.*)\\nStarted (.*)\\nUnit serial number = (.*)\\nPod1 head serial number = (.*)\\nPod Last validated (.*)\\nPod2 head serial number = (.*)\\nPod Last validated (.*)"))
(define pattern (pregexp "\\n(\\d\\d/\\d\\d/\\d\\d\\d\\d \\d\\d:\\d\\d:\\d\\d),((?:.|\\n)*?)(?:(?:\\n\\d\\d/)|$)"))
;(define pattern (pregexp "yuiop([0-9 :/]*),((?:.|\\n)*?)(?:qwert|$)"))
(define file-name (pregexp "([A-z]*8?[A-z]*)[0-9]+.*\\.log"))

;Goal: one pass, catch overlapping values

(define (seperate-rows str)
  (pregexp-match-all pattern str))

(define (pregexp-match-all pattern str)
  (let helper ([sub-start 0] [sub-end (string-length str)] [found-matches '()])
    (match (pregexp-match-positions pattern str sub-start sub-end)
      [((,start . ,end) (,g1start . ,g1end) (,g2start . ,g2end))
       (let* ([col1 (substring str g1start g1end)]
              [col2 (substring str g2start g2end)]
              [col-list (list col1 col2)])
         (helper (- end 3) sub-end (cons col-list found-matches)))]
      [#f found-matches])))

(define (process-header contents)
  (match (pregexp-match header-re contents)
    [(,fullResult . ,rest) rest]))

(define (get-method contents)
  (match contents
    [(,method . ,rest) method]))

(define (read-file-to-string file-path)
  (let ([ip (open-file-to-read file-path)])
    (on-exit (close-input-port ip)
      (get-string-all ip))))

(define (insert-header contents db)
  (let ([cols '("Method" "[Logged in User]" "Started" "[Unit serial number]" "[Pod1 head serial number]" "[Pod1 Last validated]" "[Pod2 head serial number]" "[Pod2 last validated]")])
    (apply execute-sql db
      (format "insert into Runs (~a) values (~a)"
        (join cols #\,)
        (join (map (lambda (x) "?") cols) #\,))
      contents)
    (match (execute-sql db "select last_insert_rowid()")
      [(#(,num)) num])))

(define (processfile.old table-name file-path db prepared-insert)
  (let* ([contents (read-file-to-string file-path)]
         [processed (seperate-rows contents)]
         [processed-header (process-header contents)]
         [method (get-method processed-header)]
         [run-number (insert-header processed-header db)])
    (insert-file-contents table-name processed method run-number db prepared-insert)))

(define (processfile table-name file-path db prepared-insert)
  
  (let* ([tx (make-transcoder (latin-1-codec))]
         [ip (open-file-input-port file-path (file-options) (buffer-mode block) tx)]
       ; [ip (open-file-to-read file-path)]
        [op (open-output-string)])
    (define run-number -1)
    (define method "unknown")

    (define (header)
      (let ([line (get-line ip)])
        (cond
         [(eof-object? line) (complete-header)]
         [(parse-date-line line) =>
          (lambda (x)
            (complete-header)
            (match-let* ([(,date . ,desc) x])
              (display-string desc op)
              (seen-date date)))]
         [else
          (display-string line op)
          (newline op)
          (header)])))

    (define (complete-header)
      ;; TODO: Put the header into the DB
      (get-output-string op))

    (define (parse-date-line line)
      ;; mm/dd/yyyy HH:MM:SS,
      (let ([n (string-length line)])
        (and (>= n 20)
             (eqv? (string-ref line 2) #\/)
             (eqv? (string-ref line 5) #\/)
             (eqv? (string-ref line 10) #\space)
             (eqv? (string-ref line 13) #\:)
             (eqv? (string-ref line 16) #\:)
             (eqv? (string-ref line 19) #\,)
             (char-numeric? (string-ref line 0))
             (char-numeric? (string-ref line 1))
             (char-numeric? (string-ref line 3))
             (char-numeric? (string-ref line 4))
             (char-numeric? (string-ref line 6))
             (char-numeric? (string-ref line 7))
             (char-numeric? (string-ref line 8))
             (char-numeric? (string-ref line 9))
             (char-numeric? (string-ref line 11))
             (char-numeric? (string-ref line 12))
             (char-numeric? (string-ref line 14))
             (char-numeric? (string-ref line 15))
             (char-numeric? (string-ref line 17))
             (char-numeric? (string-ref line 18))
             (cons (substring line 0 19) (substring line 20 n)))))

    (define (seen-date date)
      (let ([line (get-line ip)])
        (cond
         [(eof-object? line) (complete-line date)]
         [(parse-date-line line) =>
          (lambda (x)
            (complete-line date)
            (match-let* ([(,date . ,desc) x])
              (display-string desc op)
              (seen-date date)))]
         [else
          (newline op)
          (display-string line op)
          (seen-date date)])))

    (define (complete-line date)
      (sqlite:execute prepared-insert (list run-number method date (get-output-string op))))
    
    (on-exit (close-input-port ip)
      (header))))

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
                    [prepared-insert (sqlite:prepare db (format "insert into ~a ([Run number], Method, dateTime, desc) values (?, ?, ?, ?)" short-name))]
                    [new-table (cons short-name prepared-insert)])
              (processfile short-name path db prepared-insert)
              (process-each-file rest (cons new-table existing-tables)))]))]
      
      [() "Finished?"]))

  (define (get-name full-name)
    (let ([pattern-match (pregexp-match file-name full-name)])
      (match pattern-match
        [(,full ,name) name]
        (#f #f))))

  (define (get-second-val name list)
    (if name
        (match list
          [((,key . ,value) . ,rest)
           (if (string=? name key)
               value
               (get-second-val name rest))]
          [() #f]
          [,_ list])
        #f))

  (define (create-table table-name)
    (let (;[start-time (real-time)]
          [sql (format "create table if not exists ~a ([Run number] integer, Method text, dateTime text, desc text, foreign key([Run number]) references Runs([Unique Run Number]))" table-name)])
      (display (format "Created table: ~a\n" table-name))
      (execute-sql db sql)))
      ;(display (- (real-time) start-time))
    ;(display " Created table\n")))
  
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

  ;(let ([start-time (real-time)])
    (insert-remaining contents))
    ;(display (- (real-time) start-time))
    ;(display " insert file contents\n")))


(define (set-up-conversion folder db)
  ;; execute-sql db "PRAGMA Journal_Mode = OFF")
  ;; (execute-sql db "PRAGMA synchronous = OFF")
  (execute-sql db "create table if not exists  Runs ([Unique Run Number] integer primary key, Method text, [Logged in User] text, Started text, [Unit serial number] text, [Pod1 head serial number] text, [Pod1 Last validated] text, [Pod2 head serial number] text, [Pod2 last validated] text)")
  (fullConvert folder db))

(define (make-db-and-convert folder db-path)
  (db:start&link 'new-db db-path 'create)
  (db:stop 'new-db)
  (with-db [db db-path SQLITE_OPEN_READWRITE]
    (execute-sql db "begin transaction")
    (set-up-conversion folder db)
    (execute-sql db "end transaction")))

(define (test-convert)
  (let ([folder "C:\\Users\\AJCRAWFORD\\Documents\\Example from Praveena\\R0273091_2018.06.21_1236\\Logs\\Biomek Logs\\Logs"]
        ;[folder "C:\\Users\\AJCRAWFORD\\Documents\\Example from Praveena\\R0285647_2018.06.20_1405\\Logs\\Biomek Logs\\Logs"]
        [start-time (real-time)]
        ;[folder "C:\\Users\\AJCRAWFORD\\Documents\\Python\\testFiles\\Lots small"]
        [dest "C:\\Users\\AJCRAWFORD\\Documents\\Python\\testFiles\\Results4.db3"])
    (delete-file dest)
    (make-db-and-convert folder dest)
    (display (- (real-time) start-time))))

(test-convert)


