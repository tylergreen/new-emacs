;*****************
; Elisp Utils

(defmacro defi (name &rest body)
  "define standard interactive function"
  `(defun ,name () 
     (interactive)
     ,@body))

(defmacro disable-if-bound (fn)
  `(when (fboundp ',fn) (,fn -1)))


(defun group (n source)
  (if (endp source)
      nil
    (let ((rest (nthcdr n source)))
      (cons (if (consp rest) (subseq source 0 n ) source)
	    (group n rest)))))

(defun mkassoc (binds)
  (if (endp binds)
      nil
    (cons (cons (car binds) (cadr binds))
	  (mkassoc (nthcdr 2 binds)))))

(defmacro fn (params &rest body)
  `(lambda ,params ,@body))

; the need for this highlights a big weakness of lisp macros 
(defmacro mapm (macro &rest defs)
  "apply a macro to each list in DEFN"
  `(progn ,@(mapcar (fn (x) (cons macro x)) defs)))

; REMEMBER: nconc-- last cdr of each of the lists is changed to
; refer to the following list.
; The last of the lists is not altered

(defun load-if-exists (filename)
  (if (file-exists-p filename)
      (load-file filename)))

;; classic lisp macro example
(defmacro global-keymap (&rest bindings)
  `(progn ,@(mapcar (fn (pair)
		    `(global-set-key (kbd ,(car pair)) ',(cdr pair)))
		(mkassoc bindings))))
