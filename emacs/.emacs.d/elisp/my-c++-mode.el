(require 'flymake)

(setq auto-mode-alist
      (cons '("\\.\\(?:cpp\|cc\|hpp\|hh\\)$" . c++-mode) auto-mode-alist))
(add-hook 'c++-mode-hook
          '(lambda()
             (flymake-mode 1)

             (if (file-exists-p "GTAGS")
                 (gtags-mode 1))

             (setq tab-width 4
                   c-basic-offset 2)

             ;; (c-set-style "gnu")

             (c-turn-on-eldoc-mode)
             ))

