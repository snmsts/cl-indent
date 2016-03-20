#-asdf
(require :asdf)

(in-package :cl-user)

(defpackage cl-indent-asd
  (:use :cl :asdf))
(in-package :cl-indent-asd)

(defsystem cl-indent
  :version "0.0"
  :author "SANO Masatoshi <snmsts@gmail.com>"
  :license "MIT"
  :depends-on (:named-readtables
               :cl-ppcre
               :swank)
  :components ((:file "lispindent2" :depends-on ("tweaks"))
               (:file "tweaks"))
  :description "indent common lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-indent-test))))
