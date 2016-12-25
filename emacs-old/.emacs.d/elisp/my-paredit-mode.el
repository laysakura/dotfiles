;; Update
;; (auto-install-from-url "http://mumble.net/~campbell/emacs/paredit.el")

(require 'paredit)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-r") nil)
     (define-key paredit-mode-map (kbd "M-s") nil)))

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'python-mode-hook 'enable-paredit-mode)
(add-hook 'haskell-mode-hook 'enable-paredit-mode)
(add-hook 'yatex-mode-hook 'enable-paredit-mode)
(add-hook 'c-mode-hook 'enable-paredit-mode)
(add-hook 'c++-mode-hook 'enable-paredit-mode)
