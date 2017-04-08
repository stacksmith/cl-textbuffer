;;;; package.lisp

(defpackage #:cl-textbuffer
  (:nicknames :tb)
  (:use #:cl)
;;  (:shadow cl:position cl:delete)
  (:export
   #:gapbuf
   #:cursor-position
   #:cursor-left
   #:cursor-right
   #:cursor-adjust
   #:cursor-set
   #:insert-char
   #:delete-char
   ))

 
