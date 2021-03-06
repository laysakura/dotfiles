(defconst ELISP_DIR "~/.emacs.d/elisp")
(defun load-my-elisp (package-name)
  (load (concat ELISP_DIR "/" package-name)))
;; add "." to load-path
(setq load-path (cons "." load-path))
;; install cask packages
(require 'cask)
(cask-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;
;; body
;;;;;;;;;;;;;;;;;;;;;;;;
(load-my-elisp "my-defun")
(load-my-elisp "my-core")
(load-my-elisp "my-view")
(load-my-elisp "my-global-setting")

(load-my-elisp "my-undo-tree")
(load-my-elisp "my-company-mode")
(load-my-elisp "my-flycheck")

(load-my-elisp "my-key-bind")
(load-my-elisp "my-mouse")
