(require 'moinmoin-mode)

(setq auto-mode-alist
      (cons '("\\.moinmoin$" . moinmoin-mode) auto-mode-alist))

(add-hook 'moinmoin-mode-hook
          '(lambda()

             ))
