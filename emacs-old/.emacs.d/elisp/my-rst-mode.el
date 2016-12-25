(setq auto-mode-alist
      (cons '("\\.\\(?:rst\\)$" . rst-mode) auto-mode-alist))

(custom-set-faces
 '(rst-level-1-face ((t (:foreground "LightSkyBlue"))) t)
 '(rst-level-2-face ((t (:foreground "LightGoldenrod"))) t)
 '(rst-level-3-face ((t (:foreground "Cyan1"))) t)
 '(rst-level-4-face ((t (:foreground "chocolate1"))) t)
 '(rst-level-5-face ((t (:foreground "PaleGreen"))) t)
 '(rst-level-6-face ((t (:foreground "Aquamarine"))) t))
(custom-set-variables
 '(rst-level-face-base-light 50))

;; auto-insert
(define-auto-insert "\\.rst$" "rst-template.rst")

(add-hook 'rst-mode-hook
          '(lambda()
             (auto-complete-mode t)
             (flyspell-mode)
             (auto-fill-mode -1)
             (enable-paredit-mode)
             ))
