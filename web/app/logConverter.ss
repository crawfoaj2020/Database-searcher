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

;;Regular expressions
(define header-re (pregexp "Method = (.*)\\nLogged in user = (.*)\\nStarted (.*)\\nUnit serial number = (.*)\\nPod1 head serial number = (.*)\\nPod Last validated (.*)\\nPod2 head serial number = (.*)\\nPod Last validated (.*)"))
(define start-row-pattern (pregexp "(\\n)(\\d\\d/\\d\\d/\\d\\d\\d\\d \\d\\d:)"))
(define newline-pattern (pregexp "(.*)\\n(.*)"))
(define pattern (pregexp "yuiop([0-9 :/]*),(.*?)(?:qwert|$)"))


(define (split str divider)
  (define (helper sub-start sub-end len found-ls curStr)
    (if (>= sub-end (+ len 1))
        (let* ([last (substring str sub-start len)]
              [curStr (string-append curStr last)])
          (cons (list curStr) found-ls))
        (let ([subStr (substring str sub-start sub-end)])
          (if (string=? subStr divider)
              (let* ([found-ls (cons (list curStr) found-ls)]
                    [curStr ""]
                    [offset (- sub-end sub-start)]
                    [new-sub-start (+ sub-start offset)]
                    [new-sub-end (+ sub-end offset)])
                (helper new-sub-start new-sub-end len found-ls curStr))
              (let* ([char (substring subStr 0 1)] 
                     [curStr (string-append curStr char)]
                     [new-sub-start (add1 sub-start)]
                     [new-sub-end (add1 sub-end)])
                (helper new-sub-start new-sub-end len found-ls curStr))))))
  (let* ([len (string-length str)]
         [sub-end (string-length divider)])
    (reverse (helper 0 sub-end len '() ""))))

(define (seperate-rows str)
  (let ([start-row-replace "qwertyuiop\\2"]
        ;Currently removes internal newlines. If need, change to \\1 unique pattern \\2,
        ;then later string-replace back to newline
        [newline-replace "\\1\\2"])
    (let* ([rows-marked (pregexp-replace* start-row-pattern str start-row-replace)]
         [newlinesRemoved (pregexp-replace* newline-pattern rows-marked newline-replace)]
         [search (pregexp-match-all pattern newlinesRemoved)])
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

(define (make-string ls)
  (match ls
    [() ""]
    [(,first) (string-append "'" first "'")]
    [(,first . ,rest) (string-append "'" first "', " (make-string rest))]))

(define (read-file-to-string file-path)
  (let ([tx (make-transcoder (utf-8-codec) (eol-style lf)
                  (error-handling-mode replace))]
        [bytevector (read-file file-path)])
    (bytevector->string bytevector tx)))

(define (get-method contents)
  (match contents
    [(,method . ,rest) method]))

(define (insert-header db contents)
  (let ([comma-seperated (make-string contents)]
        [cols "Method, [Logged in User], Started, [Unit serial number], [Pod1 head serial number], [Pod1 Last validated], [Pod2 head serial number], [Pod2 last validated]"])
    (execute-sql db (format "insert into Runs (~a) values (~a)" cols comma-seperated))
    (let ([rowid (execute-sql db "select last_insert_rowid()")])
      (match rowid
        [(#(,num)) num]))))

(define (processfile new-db table-name file-path)
  (let* ([contents (read-file-to-string file-path)]
         [processed (seperate-rows contents)]
         [processed-header (process-header contents)]
         [method (get-method processed-header)]
         [run-number (insert-header new-db processed-header)])
    (insert-file-contents new-db table-name processed method run-number)))
    ;processed))

;;src-path should already have created a database
(define (fullConvert src-path dest-path)
  (let ([file-list '()])
    (with-db [new-db dest-path SQLITE_OPEN_READONLY]
     
      )))

(define (insert-file-contents db table-name contents method run-number)
  (define (format-values row-contents)
    (match row-contents
      [(,date ,desc) (format "'~a', '~a', '~a', '~a'" run-number method date desc)]))
  
  (define (insert-row row-contents)
    (let* ([values (format-values row-contents)]
           [str (format "insert into ~a ([Run number], Method, dateTime, desc) values (~a)" table-name values)])
      (execute-sql db str)))
           
  (define (insert-remaining remaining)
    (match remaining
      [(,last) (insert-row last)]
      [(,cur-row . ,rest) (begin (insert-row cur-row) (insert-remaining rest))]))
  
  (insert-remaining contents))

(let ([file "C:\\Users\\AJCRAWFORD\\Documents\\Python\\testFiles\\DeckEditor.log"]
      [demo-db "C:\\Users\\AJCRAWFORD\\Documents\\Python\\testFiles\\database2.db3"])
  (with-db [db demo-db SQLITE_OPEN_READWRITE]
    (processfile db "Errors" file)))


