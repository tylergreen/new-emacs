; workflow
; create new file (with annoying time format)
; add jekyll header to file
; create outline
; M-x org-publish
; jekyll --server

(defvar prog-blog-dir "~/blogs/progblog/org/_posts/")

(defvar jekyll-post-meta-data 
  "#+STARTUP: showall indent
#+STARTUP: hidestars
#+BEGIN_HTML
---
layout: default
title: 
---
#+END_HTML
")

(defun todays-post-file-name (post-title)
  (concat 
   prog-blog-dir
   (substring (shell-command-to-string "date +'%m-%d-%y'") 0 -1)
   "-" post-title
   "-post.Org"
  ))

(defun new-post (post-title)
  (interactive "MName of Post? ") ;;prefix argument
  (let ((filename (todays-post-file-name post-title)))
	(find-file filename)
	(insert jekyll-post-meta-data)
	))
