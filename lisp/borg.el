(defun jekyll-post-meta-data (title)
  (format
"
---
layout: post
title: %s
---
" title))

(defun todays-post-file-name (blog-directory post-title)
  (concat 
   blog-directory
   (substring (shell-command-to-string "date +'%Y-%m-%d'") 0 -1)
   "-" (replace-regexp-in-string " " "-" post-title)
   "-post.textile"
  ))

(defun new-post (blog-directory post-title)
  (let ((filename (todays-post-file-name blog-directory post-title)))
	(find-file filename)
	(insert (jekyll-post-meta-data post-title))
	))

(defun grow-post (post-title)
    (interactive "MName of Post? ") ;;prefix argument
	(new-post "~/blogs/growblog/_posts/" post-title))

(defun prog-post (post-title)
    (interactive "MName of Post? ") ;;prefix argument
	(new-post "~/blogs/tylergreen.github.com/_posts/" post-title))

(provide 'borg)
