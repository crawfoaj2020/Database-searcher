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
(import (helpers))

(define (get-page-name)
  "Convert log files")

(define (respond:error reason)
  (respond
   (match reason
     [#(browser-add) (section "Must use desktop app to convert files")]
     [#(empty-dest) (section "Must select a destination to save the newly created database")]
     [#(empty-src) (section "Must select a folder to convert")]
     [,_ (section "insert failed" `(p ,(exit-reason->english reason)))])))

(define (get-paths)
  (respond `(form (table (tr (th (p "Field")) (th (p "Value")))
                    (tr (td (p "Folder to convert")) (td (p (input (@ (name "folder") (class "path") (type "file") (webkitdirectory) (mozdirectory) (id "folder"))))))
                    (tr (td (p "Destination")) (td (p (input (@ (name "dest") (class "path") (type "file") (id "dest")))))))
              (input (@ (id "folder-path") (name "folder-path") (class "hidden")))
              (input (@ (id "dest-path") (name "dest-path") (class "hidden")))
              (script "function func(){var x = document.getElementById('folder').files[0].path;
document.getElementById('folder-path').value = x} $('.path').bind('change', func).trigger('change')")
              (script "function func(){var x = document.getElementById('dest').files[0].path;
document.getElementById('dest-path').value = x} $('.path').bind('change', func).trigger('change')")
              (p (button (@ (type "submit")) "Convert")))))

(define (dispatch)
  (let ([src (string-param "folder-path" params)]
        [dest (string-param "dest-path" params)])
    (if src
        (match (catch (do-conversion src dest))
          [#(EXIT ,reason) (respond:error reason)]
          [,value value])
        (get-paths))))

(define (do-conversion src dest)
  (unless (not (string=? "undefined" src))
    (raise `#(browser-add)))
  (unless (not (string=? "" src))
    (raise `#(empty-src)))
  (unless (not (string=? "" dest))
    (raise `#(empty-dest)))
  (respond `(p ,dest)))
        

(dispatch)
