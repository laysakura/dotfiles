(setq auto-mode-alist
      (cons (cons "Makefile.*$" 'makefile-mode) auto-mode-alist))
(add-hook 'makefile-mode-hook
          '(lambda ()
             ))

