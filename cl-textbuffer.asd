(asdf:defsystem #:cl-textbuffer
  :description "A text buffer using good ole gap technology"
  :author "stacksmith"
  :license "bsd"
  :serial t
  :depends-on (#:trivial-gray-streams)
  :components ((:file "package")
               (:file "gapbuf")))

