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


(http:include "displayQuery.ss")
(import (logConverter))

(define (get-page-name)
  "Convert log files")

(define (respond:error reason)
  (respond
   (match reason
     [#(browser-add) (section "Must use desktop app to convert files")]
     [#(empty-dest) (section "Must select a destination to save the newly created database")]
     [#(empty-src) (section "Must select a folder to convert")]
     [#(empty-name) (section "Must enter a file name")]
     [#(file-exists) (section "That file already exists in that folder. Please select a different name or destination folder")]
     [,_ (section "insert failed" `(p ,(exit-reason->english reason)))])))


(define (get-paths)
  (respond `(form (table (tr (th (p "Field")) (th (p "Value")))
                    (tr (td (p "Folder to convert")) (td (p (input (@ (name "folder") (class "path") (type "file") (webkitdirectory) (mozdirectory) (id "folder"))))))
                    (tr (td (p "Destination folder")) (td (p (input (@ (name "dest") (class "path") (type "file") (webkitdirectory) (mozdirectory) (id "dest"))))))
                    (tr (td (p "New file name")) (td (p (textarea (@ (name "name")) ""))))) 
              (input (@ (id "folder-path") (name "folder-path") (class "hidden")))
              (input (@ (id "dest-path") (name "dest-path") (class "hidden")))
              (script "function func(){var x = document.getElementById('folder').files[0].path;
document.getElementById('folder-path').value = x} $('.path').bind('change', func).trigger('change')")
              (script "function func(){var x = document.getElementById('dest').files[0].path;
document.getElementById('dest-path').value = x} $('.path').bind('change', func).trigger('change')")
              (p (button (@ (type "submit")) "Convert")))))

(define (do-conversion src dest name)
  (unless (not (string=? "undefined" src))
    (raise `#(browser-add)))
  (unless (not (string=? "" src))
    (raise `#(empty-src)))
  (unless (not (string=? "" dest))
    (raise `#(empty-dest)))
  (unless (not (string=? "" name))
    (raise `#(empty-name)))
  
  (let* ([new-file (path-combine dest name)]
         [new-file (string-append new-file ".db3")])
    (unless (not (valid-file new-file))
      (raise `#(file-exists)))
    (respond `(p "Starting conversion") `(script "location.reload()"))
    (make-db-and-convert src new-file)))


(define (conversion-complete dest name)
  (let* ([new-file (path-combine dest name)]
         [new-file (string-append new-file ".db3")])
    (respond `(p "Conversion sucessful") (link (format "addDatabase?file-path=~a" (http:percent-encode new-file)) "Add to saved databases"))
    (reset-status)))

(define (dispatch)
  (let* ([status (get-convert-status)]
         [src (string-param "folder-path" params)]
         [dest (string-param "dest-path" params)]
         [name (string-param "name" params)])
    (cond 
     [(and src (string=? status ""))
      (match (catch (do-conversion src dest name))
        [#(EXIT ,reason) (respond:error reason)]
        [,value value])]
     [(string=? status "")
      (get-paths)]
     [(string=? status "Conversion complete")
      (conversion-complete dest name)]
     [else (respond `(p ,status) `(script "location.reload()"))])))

(dispatch)

