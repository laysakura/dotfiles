(require 'kolon-mode)

(setq auto-mode-alist
      (cons '("\\.\\(?:tx\\)$" . kolon-mode) auto-mode-alist))

(add-hook 'kolon-mode-hook
          '(lambda()
             (auto-complete-mode t)
             (flyspell-mode)
             (enable-paredit-mode)
             ))
