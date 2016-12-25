(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

(setq auto-mode-alist
      (cons '("\\.jsx\\$" . jsx-mode) auto-mode-alist))

(custom-set-variables
 '(jsx-indent-level 2)
 '(jsx-cmd-options '("--add-search-path" "/home/nakatani/git/JSX/lib/js"))
 '(jsx-use-flymake t)
 '(jsx-syntax-check-mode "compile"))

(add-hook 'jsx-mode-hook
          '(lambda()
             (when (require 'auto-complete nil t)
               (auto-complete-mode t))
             ))
