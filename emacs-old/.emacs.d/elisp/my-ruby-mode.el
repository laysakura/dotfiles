(require 'ruby-electric)
(require 'ruby-block)
(require 'flymake)

; which file to be ruby-mode
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

; flymake
;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '(".+\\.rake$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()

             ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
             (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
                 (flymake-mode))

             (setq ruby-insert-encoding-magic-comment nil)

             (setq ruby-deep-indent-paren-style nil)

             (ruby-electric-mode t)
             (setq ruby-electric-expand-delimiters-list nil)

             (ruby-block-mode t)
             (setq ruby-block-highlight-toggle t)

             (inf-ruby-minor-mode)
             (inf-ruby-switch-setup)

             (robe-mode)
             ))
