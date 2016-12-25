(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.\\(?:md\\)$" . markdown-mode) auto-mode-alist))

(defun cleanup-org-tables ()
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "---+---" nil t) (replace-match "---|---"))))

(add-hook 'markdown-mode-hook
          '(lambda()
             (auto-complete-mode t)
             (flyspell-mode)
             (auto-fill-mode -1)
             (enable-paredit-mode)

             (setq truncate-lines t)

             (turn-on-orgtbl)
             (add-hook 'after-save-hook 'cleanup-org-tables  nil 'make-it-local)
             ))
