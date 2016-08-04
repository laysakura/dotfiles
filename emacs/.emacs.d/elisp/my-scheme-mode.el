(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

;; flymake
(require 'flymake)
(defvar flymake-glint-err-line-patterns '(("^\\(.+\\):\\([0-9]+\\): \\(.+\\)$" 1 2 nil 3)))
(defconst flymake-allowed-gauche-file-name-masks '(("\\.scm$" flymake-gauche-init)))
(defun flymake-gauche-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "glint" (list local-file))))

(defun flymake-gauche-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks
                                                flymake-allowed-gauche-file-name-masks))
  (setq flymake-err-line-patterns flymake-glint-err-line-patterns)
  (flymake-mode 1))

                                        ; http://www.credmp.org/index.php/2007/07/20/on-the-fly-syntax-checking-java-in-emacs/
(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))

(add-hook 'scheme-mode-hook
          '(lambda ()
             (flymake-gauche-load)
             (define-key scheme-mode-map "\C-cd" 'credmp/flymake-display-err-minibuf)
             ))
