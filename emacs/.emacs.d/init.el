;;;;;;;;;;;;;;;;;;;;;;;;
;; necessary settings
;;;;;;;;;;;;;;;;;;;;;;;;
(defconst ELISP_DIR "~/.emacs.d/elisp")
(defconst SITE_LISP_DIR "~/.emacs.d/site-lisp")
(defun load-my-elisp (package-name)
  (load (concat ELISP_DIR "/" package-name)))
;; set load-path to directories in ~/.emacs.d/site-lisp/
(defun load-all-site-lisp (toplevel)
  (setq load-path (cons toplevel load-path))  ; adds 'toplevel' to load-path
  (mapcar '(lambda (file)
             (if (and (not (string-match "\\.+$" file))  ; skips '.' & '..' directories
                      (file-directory-p file))
                 (load-all-site-lisp file)))
          (directory-files toplevel t)))
(load-all-site-lisp "~/.emacs.d/site-lisp")
;; add "." to load-path
(setq load-path (cons "." load-path))
;; install el-get packages
(load-my-elisp "my-el-get-packages")
;; install cask packages
(require 'cask)
;; to see ENV
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;
;; body
;;;;;;;;;;;;;;;;;;;;;;;;

;; MY_EMACS_MODE=light
(defun load-elisp-light ()
  ;; base
  (load-my-elisp "my-defun")
  (load-my-elisp "my-core")
  (load-my-elisp "my-view")
  (load-my-elisp "my-global-setting")

  ;; override keybindings
  (load-my-elisp "my-key-bind")
  )

;; MY_EMACS_MODE=org2tex
(defun load-elisp-org2tex ()
  ;; base
  (load-my-elisp "my-defun")
  (load-my-elisp "my-core")
  (load-my-elisp "my-view")
  (load-my-elisp "my-global-setting")

  ;; override keybindings
  (load-my-elisp "my-key-bind")

  ;; org-mode for org => tex => pdf
  (load-my-elisp "my-org-mode")
  )

;; MY_EMACS_MODE=all
(defun load-elisp-all ()
  ;; base
  (load-my-elisp "my-defun")
  (load-my-elisp "my-core")
  (load-my-elisp "my-view")
  (load-my-elisp "my-global-setting")

  ;; utility
  ;(load-my-elisp "my-wysiwyg-tex")
  (load-my-elisp "my-jisho")
  (load-my-elisp "my-auto-async-byte-compile")
  (load-my-elisp "my-minor-mode-hack")
  ;; (load-my-elisp "my-ffap")
  (load-my-elisp "my-recentf-ext")
  (load-my-elisp "my-redo+")
  (load-my-elisp "my-col-highlight")
  (load-my-elisp "my-summarye")
  (load-my-elisp "my-fold-dwim")
  (load-my-elisp "my-simple-hatena-mode")
  (load-my-elisp "my-org-export-hatena")
  (load-my-elisp "my-mew")
  (load-my-elisp "my-picture-init")

  ;; Minor Mode
  (load-my-elisp "my-doc-view-mode")
  ;; (load-my-elisp "my-yasnippet-mode")
  (load-my-elisp "my-auto-complete-mode")
  (load-my-elisp "my-flymake-mode")
  (load-my-elisp "my-gtags-mode")
  (load-my-elisp "my-paredit-mode")
  (load-my-elisp "my-eldoc-mode")
  (load-my-elisp "my-c-eldoc-mode")

  ;; Major Mode
  (load-my-elisp "my-text-mode")
  (load-my-elisp "my-info+")
  (load-my-elisp "my-python-mode")
  (load-my-elisp "my-perl-mode")
  (load-my-elisp "my-ruby-mode")
  (load-my-elisp "my-haskell-mode")
  ;; (load-my-elisp "my-erlang-mode")
  (load-my-elisp "my-c-mode")
  (load-my-elisp "my-c++-mode")
  ;; (load-my-elisp "my-r-mode")
  (load-my-elisp "my-scheme-mode")
  (load-my-elisp "my-makefile-mode")
  (load-my-elisp "my-markdown-mode")
  (load-my-elisp "my-org-mode")
  (load-my-elisp "my-rst-mode")
  ;; (load-my-elisp "my-yatex-mode")
  (load-my-elisp "my-w3m-mode")
  ;(load-my-elisp "my-twittering-mode")
  (load-my-elisp "my-gnuplot-mode")
  ;; (load-my-elisp "my-moinmoin-mode")
  (load-my-elisp "my-html-mode")
  (load-my-elisp "my-haml-mode")
  (load-my-elisp "my-javascript-mode")
  (load-my-elisp "my-jsx-mode")
  (load-my-elisp "my-kolon-mode")
  (load-my-elisp "my-plantuml-mode")

  ;; override keybindings
  (load-my-elisp "my-key-bind")
  )

(defun load-elisp-default ()
  (load-elisp-all))

;; switch which .el to load
(cond ((string= (when (memq window-system '(mac ns)) (exec-path-from-shell-copy-env "MY_EMACS_MODE")) "light") (load-elisp-light))
      ((string= (when (memq window-system '(mac ns)) (exec-path-from-shell-copy-env "MY_EMACS_MODE")) "org2tex") (load-elisp-org2tex))
      ((string= (when (memq window-system '(mac ns)) (exec-path-from-shell-copy-env "MY_EMACS_MODE")) "all")   (load-elisp-all))
      (t (load-elisp-default)))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
