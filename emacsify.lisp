(cl:in-package "https://github.com/g000001/lw-paredit")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fdefinition 'save-excursion)
    (defmacro save-excursion (&body body)
      (lw:with-unique-names (buffer)
        `(let ((,buffer (current-buffer)))
           (with-buffer-locked (,buffer)
             (buffer-save-excursion (,buffer) ,@body)))))))


(defun command-namify (symbol)
  (string-capitalize (substitute #\Space #\- (string symbol))))


(defmacro while (pred &body body)
  `(loop :while ,pred :do (progn ,@body)))


(defmacro edefun (name (&rest args) &body body)
  (if (find-if (lambda (x)
                 (typecase x
                   (cons (string-equal (car x) 'interactive))
                   (atom nil)))
               body)
      (let* ((docstring (typecase (car body)
                          (string (car body))
                          (T "")))
             (body (if (equal "" docstring)
                       body
                       (cdr body))))
        (let* ((dummy-arg '(p))
               (args (if args args dummy-arg)))
          `(defcommand ,(command-namify name) (,@args) 
                ,docstring
                ,docstring
             ,@(when (eq args dummy-arg)
                 '((declare (ignorable p))))
             (macrolet ((interactive (&optional arg)
                          (declare (ignore arg))))
               ,@body))))
      `(defun ,name (,@args) ,@body)))


(defun kill-sexp (&optional n)
  (forward-kill-form-command n))


(defun backward-delete-char-untabify (&optional n)
  (#|FIXME|# backward-delete-char n))


(defun up-list (&optional (n 1))
  (forward-up-list-command n))


(defun forward-up-list (&optional n)
  (forward-up-list-command n))


(defun backward-up-list (&optional n)
  (backward-up-list-command n))


(defun forward-char (&optional n)
  (forward-character-command n))


(defun backward-char (&optional n)
  (backward-character-command n))


(defun delete-char (&optional n)
  (delete-next-character-command n))


(defun backward-delete-char (&optional n)
  (delete-previous-character-command n))


(defun kill-line (&optional n)
  (kill-line-command n))


(defun forward-sexp (&optional n)
  (let ((point (current-point))
	(count (or n 1)))
    (form-offset point count T 0)))


(defun backward-sexp (&optional n)
  (let ((point (current-point))
	(count (- (or n 1))))
    (form-offset point count T 0)))


(let ((hcl:*packages-for-warn-on-redefinition* nil))
  (defun point ()
    (point-position)))


(defun point-min ()
  (point-position (editor::buffer-%start (current-buffer))))


(defun point-max ()
  (point-position (editor::buffer-%end (current-buffer))))


(defun insert (obj)
  (insert-string (current-point)
                 (etypecase obj
                   (string obj)
                   (character (string obj)))))


(defun beginning-of-defun (&optional n)
  (beginning-of-defun-command n))


(defun indent-sexp (&optional n)
  (indent-form-command n))


(defun parse-partial-sexp (from to)
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
Parsing stops at TO or when certain criteria are met;
 point is set to where parsing stops.
If fifth arg OLDSTATE is omitted or nil,
 parsing assumes that FROM is the beginning of a function.

Value is a list of elements describing final state of parsing:
 0. depth in parens.
 1. character address of start of innermost containing list; nil if none.
 2. character address of start of last complete sexp terminated.
 3. non-nil if inside a string.
    (it is the character that will terminate the string,
     or t if the string should be terminated by a generic string delimiter.)
 4. nil if outside a comment, t if inside a non-nestable comment,
    else an integer (the current comment nesting).
 5. t if following a quote character.
 6. the minimum paren-depth encountered during this scan.
 7. style of comment, if any.
 8. character address of start of comment or string; nil if not in one.
 9. List of positions of currently open parens, outermost first.
10. When the last position scanned holds the first character of a
    (potential) two character construct, the syntax of that position,
    otherwise nil.  That construct can be a two character comment
    delimiter or an Escaped or Char-quoted character.
11..... Possible further internal information used by `parse-partial-sexp'.

If third arg TARGETDEPTH is non-nil, parsing stops if the depth
in parentheses becomes equal to TARGETDEPTH.
Fourth arg STOPBEFORE non-nil means stop when we come to
 any character that starts a sexp.
Fifth arg OLDSTATE is a list like what this function returns.
 It is used to initialize the state of the parse.  Elements number 1, 2, 6
 are ignored.
Sixth arg COMMENTSTOP non-nil means stop after the start of a comment.
 If it is the symbol `syntax-table', stop after the start of a comment or a
 string, or after end of a comment or a string."
  (declare (ignore from to))
  (let (|0| |1| |2| |3| |4| |5| |6| |7| |8| |9| |10| |11|)
    (list |0| |1| |2| |3| |4| |5| |6| |7| |8| |9| |10| |11|)))


(defun insert-pair (arg open close)
  (insert open)
  (save-excursion
    (and arg (forward-sexp arg))
    (insert close)))


(edefun insert-parentheses (&optional arg)
  "Enclose following ARG sexps in parentheses.
Leave point after open-paren.
A negative ARG encloses the preceding ARG sexps instead.
No argument is equivalent to zero: just insert `()' and leave point between.
If `parens-require-spaces' is non-nil, this command also inserts a space
before and after, depending on the surrounding characters.
If region is active, insert enclosing characters at region boundaries.

This command assumes point is not in a string or comment."
  (interactive "P")
  (insert-pair arg #\( #\)))


(defun insert-parentheses (&optional n)
  (insert-parentheses-command n))


(defun char-before* (&optional (p (current-point)))
  (with-point ((p p))
    (let ((pb (editor::i-point-before (current-point))))
      (and pb
           (character (points-to-string p pb))))))


(defun char-before (&optional (p (current-point)))
  (save-excursion
    (typecase p
      (point 'nop)
      (integer (setq p (editor::move-buffer-point-to-offset (current-buffer) p))))
    (char-before* p)))


(defun char-after* (&optional (p (current-point)))
  (with-point ((p p))
    (let ((pa (editor::i-point-after (current-point))))
      (and pa
           (character (points-to-string p pa))))))


(defun char-after (&optional (p (current-point)))
  (save-excursion
    (typecase p
      (point 'nop)
      (integer (setq p (editor::move-buffer-point-to-offset (current-buffer) p))))
    (char-after* p)))


(defun point-at-eol (;; &optional n
                     )
  (save-excursion
    (point-position (line-end (current-point)))))


(defun newline-and-indent (&optional n)
  (indent-new-line-command n))


(defun beginning-of-line (&optional n)
  (beginning-of-line-command n))


(defun back-to-indentation (&optional n)
  (back-to-indentation-command n))


(defun delete-indentation (&optional n)
  (delete-indentation-command n))


(edefun move-past-close-and-reindent ()
  "Move past next `)', delete indentation before it, then indent after it."
  (interactive)
  (up-list 1)
  (forward-char -1)
  (while (save-excursion		; this is my contribution
	   (let ((before-paren (point)))
	     (back-to-indentation)
	     (and (= (point) before-paren)
		  (progn
		    ;; Move to end of previous line.
		    (beginning-of-line)
		    (forward-char -1)
		    ;; Verify it doesn't end within a string or comment.
                    (beginning-of-line)
                    (and (not (paredit-in-string-p))
                         (not (paredit-in-comment-p)))))))
    (delete-indentation))
  (forward-char 1)
  (newline-and-indent))


(defun move-past-close-and-reindent ()
  (move-past-close-and-reindent-command nil))


(defun skip-chars-backward (chars &optional lim)
  "Move point backward, stopping after a char not in STRING, or at pos LIM.
See `skip-chars-forward' for details.
Returns the distance traveled, either zero or negative."
  (do ()
      ((or (not (find (char-before*) chars))
           (and lim (>= (point) lim)))
       (forward-character-command 1))))


(defun skip-chars-forward (chars &optional lim)
  "Move point forward, stopping after a char not in STRING, or at pos LIM.
See `skip-chars-forward' for details.
Returns the distance traveled, either zero or negative."
  (do ()
      ((or (not (find (char-after*) chars))
           (and lim (<= (point) lim)))
       (backward-character-command 1))))


;;; *EOF*
