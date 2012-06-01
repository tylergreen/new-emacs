(defvar prog-blog-dir "~/blogs/tylergreen.github.com/_posts/")

(defun jekyll-post-meta-data (title)
  (format
"
---
layout: post
title: %s
---
" title))

(defun todays-post-file-name (post-title)
  (concat 
   prog-blog-dir
   (substring (shell-command-to-string "date +'%Y-%m-%d'") 0 -1)
   "-" (replace-regexp-in-string " " "-" post-title)
   "-post.textile"
  ))

(defun new-post (post-title)
  (interactive "MName of Post? ") ;;prefix argument
  (let ((filename (todays-post-file-name post-title)))
	(find-file filename)
	(insert (jekyll-post-meta-data post-title))
	))

(provide 'borg)
