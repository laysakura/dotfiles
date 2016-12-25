(setq auto-mode-alist
      (cons '("\\.txt$" . text-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("README" . text-mode) auto-mode-alist))

(add-hook 'text-mode-hook
          '(lambda ()
             (setq fill-column 70)
             (auto-fill-mode -1)
             ;(c-subword-mode 1)
             (flyspell-mode)
             ))

