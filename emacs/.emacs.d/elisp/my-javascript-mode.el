(setq auto-mode-alist
      (cons '("\\.js\\'" . js2-mode) auto-mode-alist))
;(add-hook 'js-mode-hook 'js2-minor-mode)

;; auto-insert
(define-auto-insert "\\.js$" "javascript-template.js")

;; color
(custom-set-faces
 '(js2-external-variable ((t (:foreground "brightyellow"))))
 )

(add-hook 'js2-mode-hook
          '(lambda()
             ; completion
             (ac-js2-mode)
             (setq ac-js2-evaluate-calls t)


             (define-key js2-mode-map (kbd "M-p") 'previous-error)
             (define-key js2-mode-map (kbd "M-n") 'next-error)
             (define-key js2-mode-map "{" 'paredit-open-curly)
             (define-key js2-mode-map "}" 'paredit-close-curly)
             (define-key js2-mode-map "(" 'paredit-open-round)
             (define-key js2-mode-map ")" 'paredit-close-round)
             (define-key js2-mode-map "[" 'paredit-open-square)
             (define-key js2-mode-map "]" 'paredit-close-square)

             (setq js2-basic-offset 2)
             (setq js2-highlight-level 3)
             (setq js2-cleanup-whitespace nil)
             (setq js2-bounce-indent-flag nil)
             ))
