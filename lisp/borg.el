; workflow
; create new file (with annoying time format)
; add jekyll header to file
; create outline
; M-x org-publish
; jekyll --server

(defvar prog-blog-dir "~/blogs/tylergreen.github.com/_posts/")

(defvar jekyll-post-meta-data 
"
---
layout: default
title: 
---
"
)

(defun todays-post-file-name (post-title)
  (concat 
   prog-blog-dir
   (substring (shell-command-to-string "date +'%Y-%m-%d'") 0 -1)
   "-" post-title
   "-post.org"
  ))

(defun new-post (post-title)
  (interactive "MName of Post? ") ;;prefix argument
  (let ((filename (todays-post-file-name post-title)))
	(find-file filename)
	(insert jekyll-post-meta-data)
	))

(provide 'borg)
