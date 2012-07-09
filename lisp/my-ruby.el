;;;; ********
;; Ruby Mode

(load-file "~/.emacs.d/lisp/ruby-electric.el")
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-hook 'ruby-mode-hook (lambda () (electric-pair-mode -1)))

(add-to-list 'compilation-error-regexp-alist-alist 
			 '(ruby-test-minitest
               "\\[\\(\\(\/[^.]+\\)+.rb\\):\\(\[0-9]+\\)\\]:"
			   1 3)
			 t ; won't overwrite
			 )

(add-to-list 'compilation-error-regexp-alist
			 'ruby-test-minitest)

;; make this so it gets automatically inserted anytime you open a file with _spec.rb at end			  
(defi minitest-template 
  (insert "require 'minitest/autorun'
require 'minitest/spec'

describe <CLASS> do
  before do
  end

  it "" do
  end
end"))


