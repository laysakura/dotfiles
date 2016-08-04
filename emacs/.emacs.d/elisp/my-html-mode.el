; Django template
(require 'django-html-mode)
(require 'django-mode)
;; (yas/load-directory "~/.emacs.d/site-lisp/el-get/django-mode/snippets")
(add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))
