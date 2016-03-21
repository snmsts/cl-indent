(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :yasi-as-library *features*))

(defpackage :cl-indent
  (:use :cl)
  (:export :indent-lines))

(defpackage :cl-indent.var
  (:use :cl)
  (:shadow :if :error :defvar))

(in-package :cl-indent.var)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (named-readtables:defreadtable :emacs
    (:merge :standard)
    (:macro-char #\? (lambda (srm char)
                       (declare (ignore char))
                       (let ((c (read-char srm nil nil nil)))
                         (when (eql c #\\)
                           (setq c (read-char srm nil nil nil)))
                         `(lambda (_)
                            ,c))))))

(named-readtables:in-readtable :emacs)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro defvar (var &optional val doc)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (export (cl:defvar ,var ,val ,doc)))))

(defvar completion-ignore-case nil)

(defmacro eval-when-compile (&body body)
  (declare (ignore body)))

(defmacro defgroup (&body body)
  (declare (ignore body)))

(defmacro interactive (&body body)
  (declare (ignore body)))

(defmacro defcustom (var val doc &rest _)
  (declare (ignore _))
  `(export (cl:defvar ,var ,val ,doc)))

(cl:defvar *put* '())
(defun put (symbol property value)
  (when (eql property 'common-lisp-indent-function)
    (push (cons (string-downcase symbol) value) *put*)))

(defun make-local-variable (symbol)
  (declare (ignore symbol)))

(defun make-variable-buffer-local (symbol)
  (declare (ignore symbol)))

(defun set-default (symbol val)
  (declare (ignore symbol val)))

(defun puthash (key value hash)
  (setf (gethash key hash) value))

(defun concat (&rest args)
  (format nil "~{~A~}" args))

(defun cl-copy-list (list)
  (copy-list list))

(defun add-hook (hook function)
  (declare (ignore hook function)))

(defmacro if (test then &rest else)
  `(cl:if ,test ,then (progn ,@else)))

(provide 'slime)
(provide 'cl-lib)

(load (merge-pathnames "contrib/slime-cl-indent.el"
                       (asdf:system-source-directory (asdf:find-system :swank))))
