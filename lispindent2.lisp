;; Original Author: Dorai Sitaram
;; Original Version: https://github.com/ds26gte/scmindent/blob/master/lispindent.lisp

#+yasi-as-library
(in-package :cl-indent)

(defvar *lisp-keywords* (or #+yasi-as-library (cl-indent.var:keywords) '()))
(defvar *tab-size* 8)
(defvar *mode* '())

#-yasi-as-library
(unless *args* ;; print help if no command-line arguments are passed to the file
  (princ "
 ___________________________________________________________________________
|   Usage:  lispindent2.0.lisp [[<file>] [--no-modify] [--no-output]]       |
|           --no-output ;; Don't output the indented code, false by default |
|           --no-modify ;; Don't modify the file, false by default          |
+---------------------------------------------------------------------------+
")
  (exit))

(defun define-with-lisp-indent-number (n syms)
  "Associates the keywords in the supplied list with the number in the first
  argument and stores them in the global *lisp-keywords* variable for later
  reference e.g (('LOOP' . 0) ('HANDLER-BIND' . 0) ('BLOCK' . 0)) "
  (dolist (sym syms)
    (let* ((x (symbol-name sym))
           (c (assoc x *lisp-keywords* :test #'string-equal)))
      (unless c
        (push (setq c (cons x nil)) *lisp-keywords*))
      (setf (cdr c) n))))

(define-with-lisp-indent-number 0
  '(block
    handler-bind
    loop))

(define-with-lisp-indent-number 1
  '(case
    defpackage do-all-symbols do-external-symbols dolist do-symbols dotimes
    ecase etypecase eval-when
    flet
    handler-case
    labels lambda let let* let-values
    macrolet
    prog1
    typecase
    unless unwind-protect
    when with-input-from-string with-open-file with-open-socket
    with-open-stream with-output-to-string))

(define-with-lisp-indent-number 2
  '(assert
    defun destructuring-bind do do*
    if
    multiple-value-bind
    with-slots))

#|
  Basically returns the position of the end of this token(symbol,variable e.t.c), e.g:
    (past-next-token " this that " 0 8)      => 0
    (past-next-token " this that " 1 8)      => 5
    (past-next-token "this       that " 0 8) => 4
    (past-next-token "this       that " 1 8) => 4
    (past-next-token "this       that " 1 3) => 3
    (past-next-token "this#hash" 0 20)       => 4
    (past-next-token "th(is#hash" 0 20)      => 3
    (past-next-token "th\\(is#hash" 0 20)    => 6
    (past-next-token "thishash" 0 20)        => ERROR!!
   if not found, it returns n.
|#

(defun past-next-token (str current n)
  (loop
     with escapep = nil
     for i from current below n
     for c = (char str i) ;; c holds the current character.
     do (cond (escapep (setq escapep nil)) ;; if true set to false
              ((char= c #\\) (setq escapep t))
              ((char= c #\#)
               (let ((j (+ i 1)))
                 (if (>= j n) (return i)
                     (let ((c (char str j)))
                       (cond ((char= c #\\) (setq escapep t i j)) ;; found character literal, stop.
                             (t (return i)))))))
              ((member c '(#\Space #\Tab #\( #\) #\[ #\] #\" #\' #\` #\, #\; #\} #\{))
               ;; (format t "token: `~a`" c)
               (return i)))
     finally (return n)))

(defun lisp-indent (str &optional (possible-keyword-p t))
  "Returns the indentation information for the keyword if it is *lisp-keywords*. If it
 starts with 'def', it's indent value is 0. if it has a colon preceding it, the
 rest of the string is tested recursively to see whether it is a keyword."
  (cond ((cdr (assoc str *lisp-keywords* :test #'string-equal)))
        ((search "def" str :test #'char-equal) 0)
        (possible-keyword-p
         (let ((colon-pos (position #\: str :from-end t)))
           (when colon-pos
             (lisp-indent (subseq str (1+ colon-pos)) nil))))))

(defun literal-token-p (str)
  (let ((colon-pos (position #\: str)))
    (if colon-pos
        (zerop colon-pos)
        (let ((read-token
               (ignore-errors
                 (read-from-string str))))
          (or (characterp read-token)
              (numberp read-token)
              (stringp read-token))))))

;; (trace lisp-indent literal-token-p read-from-string past-next-token)

(defstruct blk ;;block
  spaces-before
  num-aligned-subforms
  line
  block-type
  indent
  (num-finished-subforms 0))

#|
 (calc-subindent "eval-when (condition that)" 0 20) ;; ==> 2
 (calc-subindent "some-func (condition that)" 0 20) ;; ==> 11
 (calc-subindent "some-func   (condition that)" 0 20) ;; ==> 11
    ;; since the function uses past-next-token, it'll always return the position
    ;; of the first space which causes wrong indentation.
 (calc-subindent "if   (condition that)" 0 20) ;; ==> 2
 (calc-subindent "'(   (condition that)" 0 20) ;; ==> 1
 (calc-subindent ":define   (condition that)" 0 20) ;; ==> 2
|#

(defun calc-subindent (str i n)
  (let* ((j (past-next-token str i n)) ;; store position of start of the next token
         (num-aligned-subforms 0)
         (left-indent
          (if (= j i)
              1 ;; no token found. there was a space at the start of the line.
            (let ((token (subseq str i j))) ;; store the function name
              (if (or (and (find :accept-other-brackets *mode*)
                           ;; Treat curly brackets and square brackets as literal lists with an indentation of 1
                           (member (char str (if (= i 0) i (- i 1))) '(#\{ #\[)))
                      (and (>= i 2) (member (char str (- i 2)) '(#\' #\`))))
                  1 ;; if it's a list literal set indent value to 1
                (let ((nas (lisp-indent token))) ;; get the functions indent value. returns -1 if the token is not in *lisp-keywords*
                  (cond ((numberp nas) ;; the token is a lisp keyword.
                         (setq num-aligned-subforms nas)
                         2)
                        ((literal-token-p token) 1) ;; found literal, the indent value defaults to 1
                        ((= j n) 1) ;; first argument probably in next lines
                        (t (+ (- j i) 2))))))))) ;; assumes that the first argument starts after the space at the end of the token
    (values left-indent num-aligned-subforms (1- j)))) ;; j stores where we last stopped processing

(defun num-leading-spaces (str)
  (loop
     with i = 0
     with n = (length str)
     do 
       (when (>= i n)
         (return 0))
       (case (char str i)
         (#\Space (incf i))
         (#\Tab (incf i *tab-size*))
         (t (return i)))))

(defun string-trim-blanks (s &optional type)
  "Remove leading and trailing whitespace even in a string."
  (funcall (case type
             (:right #'string-right-trim)
             (:left #'string-left-trim)
             (t #'string-trim))
           '(#\Space #\Tab #\Newline #\Return) s))

(defun indent-lines (&key (in *standard-input*) (out *standard-output*))
  (loop
     with left-i = 0
     with block-stack = '()
     with stringp = nil
     with multiline-commentp = nil
     for line-num from 1
     for curr-line = (or (read-line in nil) (return)) ;; get the current line stop if at the end
     for leading-spaces = (num-leading-spaces curr-line) ;; find the number of leading spaces
     for curr-left-i ;; will store the indent level
       = (cond ((or stringp multiline-commentp) ;; if in a string, the indent level stays the same
                leading-spaces)
               ((null block-stack)
                (if (= left-i 0) ;; the value in left-i serves as the zero_level
                    (setq left-i leading-spaces)
                    left-i))
               (t (let* ((blk (car block-stack))
                         (nas (blk-num-aligned-subforms blk)) ;; num-aligned-subforms is not really necessary since it'll be 2
                         (nfs (blk-num-finished-subforms blk))) ;; num-finished-subforms is used to detect whether we have found an if-clause
                    (+ (blk-spaces-before blk)
                       (or
                        (when (< nfs nas)
                          (incf (blk-num-finished-subforms blk))
                          2) ;; extra width is used to make the if-clause have more indentation than the else clause
                        0)))))
     do
       (cond
         (multiline-commentp
          (setq curr-line (string-trim-blanks curr-line :right))
          (princ curr-line out))
         (stringp
          (princ curr-line out))
         (t
          (setq curr-line (string-trim-blanks curr-line)) ;; remove leading to be added according to the indentation level
          (dotimes (k curr-left-i)
            (write-char #\Space out)) ;; print leading spaces corresponding to the indent level.
          (princ curr-line out)))
       (loop ;; walk the string character by character
          with str-len = (length curr-line)
          with escapep
          with inter-word-space-p
          for i from 0 to (1- str-len)
          for c = (char curr-line i)
          do (cond (escapep (setq escapep nil))
                   ((char= c #\\) (setq escapep t))
                   (stringp (when (char= c #\") (setq stringp nil))) ;; found a quote, end of string or multiline comment
                   (multiline-commentp (when (char= c #\|) (setq multiline-commentp nil))) ;; found a pipe, assume the multiline comment ends here
                   ((char= c #\;) (return)) ;; found a comment, skip the rest of the line
                   ((char= c #\") (setq stringp t)) ;; switch the string predicate to true, found a string or comment.
                   ((char= c #\|) (setq multiline-commentp t)) ;; switch the multiline predicate to true, found a multiline comment.
                   ((member c '(#\Space #\Tab) :test #'char=)
                    (unless inter-word-space-p ;; inter-word-space-p helps us ignore consecutive spaces that would give a false detection of an if-clause.
                      (setq inter-word-space-p t)
                      (let ((blk (car block-stack)))
                        (when blk
                          (incf (blk-num-finished-subforms blk))))))
                   ((member c '(#\( #\[ #\{) :test #'char=)
                    (setq inter-word-space-p nil)
                    (multiple-value-bind (left-indent num-aligned-subforms j)
                        (calc-subindent curr-line (1+ i) str-len)
                      ;; (format t "line(~a,~a): `~a`~%" (+ curr-left-i i) j (subseq curr-line i j))
                      (push
                       (make-blk :spaces-before (+ i curr-left-i left-indent)
                                 :num-aligned-subforms num-aligned-subforms
                                 :line line-num
                                 :block-type c)
                       block-stack)
                      (setq i j)))
                   ((member c '(#\) #\] #\}) :test #'char=)
                    (setq inter-word-space-p nil)
                    (cond (block-stack (pop block-stack))
                          (t (setq left-i 0))))
                   (t (setq inter-word-space-p nil))))
       (write-char #\Linefeed out))) ;; print the line with the correct indentation and a newline(terpri)

#-yasi-as-library
(progn
  (defvar *file-name* (first *args*))
  (defvar *output-to-stdout* (not (member "--no-output" *args* :test #'string-equal)))
  (defvar *modify-file* (not (member "--no-modify" *args* :test #'string-equal)))
  ;; Get lisp keywords stored in your home directory and add them to the *lisp-words* list.
  (with-open-file (i (merge-pathnames ".lispwords" (user-homedir-pathname))
                     :if-does-not-exist nil)
    (when i
      (loop
         (let ((w (or (read i nil) (return))))
           (define-with-lisp-indent-number (car w) (cdr w))))))
  (and (search ".clj" *file-name*)
       (= 4 (- (length *file-name*);; If the file is a Clojure file.
               (search ".clj" *file-name* :from-end t)))
       (pushnew :accept-other-brackets *mode*))
  (with-open-file (file-with-code *file-name*
                                  :direction :input)
    (with-open-file (indented-file "indented-file.lisp"
                                   :direction :output :external-format :unix)
      (when *output-to-stdout*
        (setf indented-file (make-broadcast-stream indented-file *standard-output*)))
      (indent-lines :in file-with-code :out indented-file)))
  (when *modify-file*
    (delete-file *file-name*)
    (rename-file "indented-file.lisp" *file-name*)))
