;; My .emacs file for EMACS 24
;; to find unbalanced parens -- go to the end of the file and type C-u C-M-u.
;; This will move you to the beginning of the first defun that is unbalanced. 

;; See local-config.el for local configurations
;;

;; for ECB
(setq stack-trace-on-error nil)

; ***********
; My extensions
; put a file name '.nosearch' in directories you do not want to be loaded
 (let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-subdirs-to-load-path))

(load-file "~/.emacs.d/lisp/my-utils.el")

;*******************
; Parenthesis Matching

(if (= 24 emacs-major-version) (electric-pair-mode t))
(show-paren-mode t)
(setq show-paren-delay 2) ; delay in seconds

(setq-default js2-basic-offset 2)
(setq-default tab-width 2)
(setq tab-stop-list (number-sequence 2 120 2))
(setq-default indent-tabs-mode nil)


;*******
; Shell Mode

(dirtrack-mode t)

;; use word wrapping everywhere
(global-visual-line-mode 1)

; allow exit of emacs client without closing any buffers
(setq server-kill-new-buffers nil)

(setq make-backup-files nil)

(setq auto-save-mode nil)

;; (setq backup-directory-alist
;; 	  `((".*" . ,temporary-file-directory)))

;; (setq auto-save-file-name-transforms
;; 	  `((".*" ,temporary-file-directory t)))

(mapm disable-if-bound
      (menu-bar-mode)
      (toggle-scroll-bar)
      (tool-bar-mode)
      (osx-key-mode)
      )

(defun mac-setup ()
  )

(defun linux-setup ()
  (setq x-select-enable-clipboard t)
  (disable-if-bound menu-bar-mode)
  )

(cond ((eq system-type 'darwin)
       (mac-setup))
      ((member system-type '(gnu/linux linux))
       (linux-setup)))

; *************
; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
	(end-of-buffer)
	(eval-print-last-sexp)))

(el-get 'sync)

(setq my-packages
	  '(clojure-mode
		coffee-mode
		color-theme
		color-theme-solarized
		ecb
		el-get
		google-maps
		graphviz-dot-mode
		haskell-mode
		haskell-mode-exts
		js-comint
		js2-mode
		json
		lua-mode
		mustache-mode
		ruby-mode
		ruby-compilation
		ruby-end
;;		ruby-electric
		rvm
		shell-current-directory
		smex
		swank-clojure
		vkill 
		yasnippet
;		yaml-mode
))

(el-get 'sync my-packages)

;;;;;;;;;;;;;;;;;
; Windowing Config 

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook
	  'ansi-color-for-comint-mode-on)

(if window-system
	(color-theme-solarized-dark) ; (color-theme-gnome2)
  (color-theme-hober) ;     (color-theme-calm-forest)
  )

(unless (transient-mark-mode)
  (transient-mark-mode))

;*****************
; Libraries

(mapc 'require
      '(cl
		ibuffer 
		ido
		tramp
		sql
		))

(ido-mode)
(setq ido-auto-merge-delay-time 5)

;****************
; Emacs Config

;;; Shut up compile saves
(setq compilation-ask-about-save nil)
;;; Don't save *anything*
(setq compilation-save-buffers-predicate '(lambda () nil))

; never see that stupid warning again
; can also pass nil
(setq enable-local-variables :safe)

; don't make me type yes and no
(fset 'yes-or-no-p 'y-or-n-p)

(mapc (fn (pair) (add-to-list 'auto-mode-alist pair))
      (mkassoc '(
 		  "\\.cljs\\'" clojure-mode
		  "\\.pl\\'" prolog-mode
 		  "\\.txt\\'" auto-fill-mode
		  "\\.py\\'" python-mode
		  "\\.clj\\''" clojure-mode
		  "\\.el\\'" emacs-lisp-mode
		  "\\.yml\\'" yaml-mode
		  )))


(mapc 'global-unset-key '( "\C-_"
			  ))

(global-keymap 
 "C-q" backward-kill-word
 "C-x C-j" kill-this-buffer
 "C-x j" kill-this-buffer
 "C-k" kill-line-or-region
; "C-w" ;available
 "C-." other-frame
 "C-," previous-multiframe-window
 "C-x C-u" undo
 "C-x C-n" next-line
 "M-g" goto-line
 "M-j" shell
 "C-c C-q" quote-prev
 "M-u" upcase-prev
 "M-c" cap-prev
 "C-x C-b" ibuffer
 "M-k" kill-ring-save
 "M-SPC" set-mark-command
 "M-w" ecb-toggle-between
 "M-x" smex
 "C-x f" find-file-in-repository
 "C-x C-f" find-file-in-repository
; M-f forward-whitespace
; M-b backward-whitespace ; not written
 )

(defi datahand
    (global-keymap
	"M-SPC" set-mark-command
	 "M-u" windmove-up
	 "M-m" windmove-down
	 "M-h" windmove-left
	 "M-'" windmove-right
	))

(defi kineses
  (global-keymap
   "<up>" windmove-up
   "<down>" windmove-down
   "<right>" windmove-right
   "<left>" windmove-left
   ))


(defi normal-keyboard
  (global-keymap
   "<up>" previous-line
   "<down>" next-line
   "<right>" forward-char
   "<left>" backward-char
   ))

(kineses)

(put 'kill-ring-save 'interactive-form
	 '(interactive 
	   (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (line-beginning-position) (line-beginning-position 2)))))

(defi kill-line-or-region
  (if (use-region-p)
	  (kill-region (region-beginning) (region-end))
	(kill-line)))

(defun disable (commands)
  (mapc (fn (x) (put x 'disabled t))
	commands))

(disable '(upcase-region
	   downcase-region
	   ))

; *********
; Custom Commands

(defi dot
  (find-file "~/.emacs.d/init.el"))

(defi bash
  (find-file "~/.bashrc"))

(defi quote-prev
  (save-excursion
    (insert "\"")
    (backward-word)
    (insert "\""))
  (forward-char))

(defi upcase-prev
  (backward-word)
  (upcase-word 1))

(defi cap-prev 
  (backward-word)
  (capitalize-word 1))

;; Customize this for you own use -- straight from emacs-fu
(setq ibuffer-saved-filter-groups
  '((("default"      
	  ("Org" ;; all org-related buffers
	   (mode . org-mode))
	  ("Shells"
	   (mode . shell-mode))
	  ("magit"
	   (mode . magit-status-mode)
	   ("Programming" ;; prog stuff not already in MyProjectX
		(or
                (mode . c-mode)
                (mode . perl-mode)
                (mode . python-mode)
                (mode . emacs-lisp-mode)
                ;; etc
                )) 
	   ("ERC"   (mode . erc-mode)))))))

(add-hook 'ibuffer-mode-hook
		  (fn () (ibuffer-switch-to-saved-filter-groups "default")))

; *********** 
; Major Modes 

(defun coffee-custom ()
  (set (make-local-variable 'tab-width) 2))

(if (fboundp 'coffee-mode)
    (add-hook 'coffee-mode-hook
	      '(lambda () (coffee-custom))))

;; Blogging
(load "~/.emacs.d/lisp/borg.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defi ecb-toggle-between
  (if (equal (buffer-name) " *ECB Directories*" )
	  (ecb-goto-window-edit-last)
	(ecb-goto-window-directories)
  ))

;; **********
;; Language specific extensions
;;(load-file "~/.emacs.d/lisp/my-ruby.el")

