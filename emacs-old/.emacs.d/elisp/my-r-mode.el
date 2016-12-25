(load "../site-lisp/ess-5.3.10/lisp/ess-site")
(autoload 'ess-rdired "ess-rdired" "View *R* objects in a dired-like buffer." t)

(require 'auto-complete-acr)

(add-hook 'R-mode-hook
          '(lambda()
             (auto-complete-mode 1)
             (c-turn-on-eldoc-mode)
             (setq default-process-coding-system
                   '(utf-8 . utf-8))
             ))

